(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xmlencoding

exception InvalidInput
exception LexerError of string
exception UnknownToken of string
exception UnknownEntity of string
exception InvalidChar of int
exception Finished

type data =
  | UCS4 of Xmlencoding.ucs4
  | EOB

type name = string

type external_id = string * string

type quantify = [
| `One
| `Plus
| `Quest
| `Star
]

type contentspec_children = [
| `Choice of cp list * quantify
| `Seq of cp list * quantify
]
and cp = [
| `Name of name * quantify
| `Choice of cp list * quantify
| `Seq of cp list * quantify
]

type contentspec = [
| `EMPTY
| `ANY
| `Mixed of name list                 (* remember about #PCDATA *)
| `Children of contentspec_children
]

type atttype = [
| `CDATA
| `ID
| `IDREF
| `IDREFS
| `ENTITY
| `ENTITIES
| `NMTOKEN
| `NMTOKENS
| `NOTATION of name list
| `ENUMERATION of string list
]

type defaultdecl = [
| `REQUIRED
| `IMPLIED
| `FIXED of string
| `Default of string
]

type parameter_entity_value = [
| `EntityValue of string
| `ExternalID of external_id
]

type entity_value = [
| `EntityValue of string
| `ExternalID of external_id
| `UnparsedExternalID of external_id * name
]

type entitydecl = [
| `ParameterEntity of name * parameter_entity_value
| `Entity of name * entity_value
]

type intsub = [
| `Elementdecl of name * contentspec
| `DeclSect of name
| `AttlistDecl of name * (name * atttype * defaultdecl) list
| `EntityDecl of entitydecl
| `NotationDecl of name * external_id
| `PI  of string * string
| `Comment of string
]

type dtd = {
  dtd_name : name;
  dtd_external_id : external_id option;
  dtd_intsubset : intsub list;
}

type production =
  | StartElement of name * (name * string) list
  | EndElement of name
  | Pi of name * string
  | Comment of string
  | Whitespace of string
  | Text of string
  | Doctype of dtd
  | EndOfBuffer
  | EndOfData

type parser_t = {
  is_parsing : bool;
  finish : bool;
  i : int;
  buffer : string;
  encoding : string;
  fdecoder : string -> int -> Xmlencoding.t;
  fencoder : int -> char list;
  fencoder_error : Xmlencoding.ucs4 list -> string;
  fparser : parser_t -> parser_t * production;
  entity_resolver : string -> string;
}
and lstream = | Lexer of (parser_t -> data -> lstream)
              | SwitchDecoder of string * (string -> int -> Xmlencoding.t)
              | Token of production * (parser_t -> data -> lstream) option * bool

let rec ignore_eob nextf state = function
  | UCS4 ucs4 -> nextf state ucs4
  | EOB -> Lexer (ignore_eob nextf)

let rec skip_blank nextf =
  ignore_eob (fun state ucs4 ->
                if Xmlchar.is_space ucs4 then
                  Lexer (skip_blank nextf)
                else
                  nextf state ucs4
             )

let after_blank nextf =
  ignore_eob (fun _state ucs4 ->
                if Xmlchar.is_space ucs4 then
                  Lexer (skip_blank nextf)
                else
                  raise (LexerError "expected space")
             )

let next_char nextf =
  Lexer (ignore_eob nextf)

let get_word nextf state ucs4 =
  let is_ascii ucs4 =
    if ucs4 < 0xFF then
      match Char.chr ucs4 with
        | 'A'..'Z'
        | 'a'..'z' -> true
        | _ -> false
    else
      false
  in
  let buf = Buffer.create 30 in
  let rec aux_get_word state ucs4 =
    if is_ascii ucs4 then (
      Buffer.add_char buf (Char.chr ucs4);
      next_char aux_get_word
    )
    else
      let word = Buffer.contents buf in
        Buffer.reset buf;
        nextf word state ucs4
  in
    aux_get_word state ucs4

let expected_char ucs4 f _state ucs4' =
  if ucs4' = ucs4 then f
  else
    raise (LexerError (Printf.sprintf "expected char %c, met %c"
                         (Char.chr ucs4) (Char.chr ucs4')))

let fencoder state buf ucs4 =
  List.iter (Buffer.add_char buf) (state.fencoder ucs4)

let fencoder_string state uclist =
  let buf = Buffer.create (List.length uclist) in
    List.iter (fencoder state buf) uclist;
    Buffer.contents buf

(*
 * Gives a quoted string
 * ("'" [^"'"]* "'") | ('"' [^'"']* '"')
 *)
let parse_string nextf _state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_text qt state ucs4 =
    if ucs4 = qt then
      let str = Buffer.contents buf in
        Buffer.reset buf;
        nextf str
    else (
      fencoder state buf ucs4;
      next_char (get_text qt)
    )
  in
    if ucs4 = Xmlchar.u_quot || ucs4 = Xmlchar.u_apos then
      next_char (get_text ucs4)
    else
      raise (LexerError "expected string")

(*
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar |
 *                  Extender
 * [5] Name     ::= (Letter | '_' | ':') (NameChar)*
 *)
let parse_name nextf state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_name state ucs4 =
    if Xmlchar.is_namechar ucs4 then (
      fencoder state buf ucs4;
      next_char get_name
    ) else (
      let name = Buffer.contents buf in
        Buffer.reset buf;
        nextf name state ucs4
    )
  in
    if Xmlchar.is_first_namechar ucs4 then (
      fencoder state buf ucs4;
      next_char get_name
    )
    else (
      fencoder state buf ucs4;
      raise (LexerError ("invalid name '" ^ Buffer.contents buf ^ "'"))
    )

(*
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
 *                  CombiningChar | Extender
 * [7] Nmtoken  ::= (NameChar)+
 * [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
 *)
let parse_nmtoken nextf state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_name state ucs4 =
    if Xmlchar.is_namechar ucs4 then (
      fencoder state buf ucs4;
      next_char get_name
    ) else (
      let name = Buffer.contents buf in
        Buffer.reset buf;
        nextf name state ucs4
    )
  in
    if Xmlchar.is_namechar ucs4 then (
      fencoder state buf ucs4;
      next_char get_name
    )
    else (
      fencoder state buf ucs4;
      raise (LexerError ("invalid name '" ^ Buffer.contents buf ^ "'"))
    )

(*
 * [66] CharRef ::= '&#' [0-9]+ ';'
 *                | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]
 *)
let parse_charref nextf state ucs4 =
  let parse_decimal_charref nextf state ucs4 =
    let rec get_decimal acc _state ucs4 =
      if ucs4 >= Xmlchar.u_0 && ucs4 <= Xmlchar.u_9 then
        next_char (get_decimal (acc * 10 + (ucs4 - Xmlchar.u_0)))
      else if ucs4 = Xmlchar.u_semicolon then
        nextf acc
      else
        raise (LexerError "malformed character reference")
    in
      get_decimal 0 state ucs4
  in
  let parse_hexacimal_charref nextf =
    let rec get_decimal acc state ucs4 =
      if ucs4 >= Xmlchar.u_0 && ucs4 <= Xmlchar.u_9 then
        next_char (get_decimal (acc * 16 + (ucs4 - Xmlchar.u_0)))
      else if ucs4 >= Xmlchar.of_char 'A' && ucs4 <= Xmlchar.of_char 'F' then
        next_char
          (get_decimal (acc * 16 + (ucs4 - Xmlchar.of_char 'A' + 10)))
      else if ucs4 >= Xmlchar.of_char 'a' && ucs4 <= Xmlchar.of_char 'f' then
        next_char
          (get_decimal (acc * 16 + (ucs4 - Xmlchar.of_char 'a' + 10)))
      else if ucs4 = Xmlchar.u_semicolon then
        nextf state acc
      else
        raise (LexerError "malformed character reference")
    in
      next_char (get_decimal 0)
  in
    if ucs4 = Xmlchar.of_char 'x' then
      parse_hexacimal_charref (fun state ucs4 ->
                                 if Xmlchar.is_xmlchar ucs4 then
                                   nextf state ucs4
                                 else
                                   raise (InvalidChar ucs4)
                              )
    else
      parse_decimal_charref
        (fun ucs4 ->
           if Xmlchar.is_xmlchar ucs4 then
             nextf state ucs4
           else
             raise (InvalidChar ucs4)
        ) state ucs4

  

(*
 * [68] EntityRef   ::= '&' Name ';'        [WFC: Entity Declared]
 *                                          [VC: Entity Declared]
 *                                          [WFC: Parsed Entity]
 *                                          [WFC: No Recursion]
 *)
let parse_entityref buf nextf state ucs4 =
  parse_name
    (fun name state ucs4 ->
       if ucs4 = Xmlchar.u_semicolon then (
         (match name with
            | "lt" -> fencoder state buf Xmlchar.u_lt
            | "gt" -> fencoder state buf Xmlchar.u_gt
            | "apos" -> fencoder state buf Xmlchar.u_apos
            | "quot" -> fencoder state buf Xmlchar.u_quot
            | "amp" -> fencoder state buf Xmlchar.u_amp
            | other ->
                let str = state.entity_resolver other in
                  Buffer.add_string buf str
         );
         nextf
       ) else
         raise
           (LexerError
              ("invalid reference: expecting ':' after '&" ^ name ^ "'"))
    ) state ucs4

(*
 * [69] PEReference ::= '%' Name ';'        [VC: Entity Declared]
 *                                          [WFC: No Recursion]
 *                                          [WFC: In DTD]
 *)
let parse_PEreference nextf state ucs4 =
  parse_name (fun name _state ucs4 -> if ucs4 = Xmlchar.u_semicolon then
                nextf name
              else
                raise (LexerError "expected %Name;")
             ) state ucs4

(*
 * [67] Reference   ::= EntityRef | CharRef
 *)
let parse_reference buf nextf state ucs4 =
  if ucs4 = Xmlchar.u_sharp then
    next_char (parse_charref
                 (fun state ucs4 ->
                    fencoder state buf ucs4;
                    nextf))
  else
    parse_entityref buf nextf state ucs4

(*
 * [9] EntityValue   ::= '"' ([^%&"] | PEReference | Reference)*
 *                       '"'
 *                       | "'" ([^%&'] | PEReference | Reference)*
 *                       "'"
 * [69] PEReference  ::= '%' Name ';
 *)
let parse_entity_value nextf _state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_value qt state ucs4 =
    if ucs4 = qt then
      let value = Buffer.contents buf in
        Buffer.reset buf;
        nextf value
    else if ucs4 = Xmlchar.u_percent then
      next_char (parse_name (fun name state ucs4 ->
                               Buffer.add_string buf name;
                               get_value qt state ucs4))
    else if ucs4 = Xmlchar.u_amp then
      next_char (parse_reference buf (next_char (get_value qt)))
    else (
      fencoder state buf ucs4;
      next_char (get_value qt)
    )
  in
    if ucs4 = Xmlchar.u_apos || ucs4 = Xmlchar.u_quot then
      next_char (get_value ucs4)
    else (
      raise (LexerError "malformed attribute value")
    )

(*
 * [10] AttValue    ::= '"' ([^<&"] | Reference)* '"'
 *                      | "'" ([^<&'] | Reference)* "'"
 *)
(*  Implements also (partially) 3.3.3 Attribute-Value Normalization *)
let parse_attvalue nextf _state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_value qt state ucs4 =
    if ucs4 = qt then
      let value = Buffer.contents buf in
        Buffer.reset buf;
        next_char (nextf value)
    else if ucs4 = Xmlchar.u_amp then
      next_char (parse_reference buf (next_char (get_value qt)))
    else if ucs4 = Xmlchar.u_lf || ucs4 = Xmlchar.u_tab then (
      fencoder state buf ucs4;
      next_char (get_value qt)
    )
    else if ucs4 = Xmlchar.u_lt then
      raise (LexerError "'<' disallowed in attribute value")
    else (
      fencoder state buf ucs4;
      next_char (get_value qt)
    )
  in
    if ucs4 = Xmlchar.u_apos || ucs4 = Xmlchar.u_quot then
      next_char (get_value ucs4)
    else (
      raise (LexerError "malformed attribute value")
    )

(*
 * [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]* )
 *)
let parse_text state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_text state = function
    | UCS4 ucs4 ->
        if ucs4 = Xmlchar.u_lt then (
          let text = Buffer.contents buf in
            Buffer.reset buf;
            Token (Text text, None, false)
        ) else if ucs4 = Xmlchar.u_amp then
          next_char (parse_reference buf (Lexer get_text))
        else if ucs4 = Xmlchar.u_closebr then
          next_char
            (fun state ucs42 ->
               if ucs42 = Xmlchar.u_closebr then
                 next_char
                   (fun _state ucs43 ->
                      if ucs43 = Xmlchar.u_gt then
                        raise (LexerError "']]>' is not allowed in text")
                      else (
                        fencoder state buf ucs4;
                        fencoder state buf ucs42;
                        get_text state (UCS4 ucs43)
                      ))
               else (
                 fencoder state buf ucs4;
                 get_text state (UCS4 ucs42)
               ))
        else (
          fencoder state buf ucs4;
          Lexer get_text
        )
    | EOB ->
        let text = Buffer.contents buf in
          Buffer.reset buf;
          Token (Text text, None, true)
  in
    fencoder state buf ucs4;
    Lexer get_text

let parse_whitespace state ucs4 =
  let buf = Buffer.create 10 in
  let rec get_spaces state = function
    | UCS4 ucs4 ->
        if Xmlchar.is_space ucs4 then (
          fencoder state buf ucs4;
          Lexer get_spaces
        )
        else
          let text = Buffer.contents buf in
            Buffer.reset buf;
            Token (Whitespace text, None, false)
    | EOB ->
        let text = Buffer.contents buf in
          Buffer.reset buf;
          Token (Whitespace text, None, true)
  in
    fencoder state buf ucs4;
    Lexer get_spaces

(*
 * [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
 *)
let parse_comment nextf _state ucs4 =
  let buf = Buffer.create 30 in
    if ucs4 = Xmlchar.u_dash then
      let rec get_comment state ucs41 =
        if ucs41 = Xmlchar.u_dash then
          next_char
            (fun _state ucs42 ->
       if ucs42 = Xmlchar.u_dash then
         next_char
           (fun _state ucs43 ->
              if ucs43 = Xmlchar.u_gt then
                let comment = Buffer.contents buf in
                  Buffer.reset buf;
                  nextf comment
              else
                raise (LexerError
                         "-- is not allowed inside comment")
           )
       else (
         fencoder state buf ucs41;
         fencoder state buf ucs42;
         next_char get_comment
       ))
        else (
          fencoder state buf ucs41;
          next_char get_comment
        )
      in
        next_char get_comment
    else
      raise (LexerError "Malformed cooment")

(*
 * [16] PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char* )))? '?>'
 * TODO:
 * [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
 *)
let parse_pi nextf =
  parse_name
    (fun target _state ucs4 ->
       let rec get_pi_content acc _state ucs41 =
         if ucs41 = Xmlchar.u_quest then
           next_char (fun _state ucs42 ->
                        if ucs42 = Xmlchar.u_gt then
                          nextf target (List.rev acc)
                        else
                          next_char  (get_pi_content (ucs42 :: ucs41 :: acc))
                     )
         else
           next_char (get_pi_content (ucs41 :: acc))
       in
         if Xmlchar.is_space ucs4 then
           next_char
             (fun state ucs4 -> skip_blank (get_pi_content []) state (UCS4 ucs4))
         else if ucs4 = Xmlchar.u_quest then
           next_char
             (fun _state ucs4 ->
                if ucs4 = Xmlchar.u_gt then
                  nextf target []
                else
                  raise (LexerError "Invalid syntax of PI")
             )
         else
           raise (LexerError "Invalid syntax of PI")
    )

(*
 * [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
 *)
let parse_system_literal nextf state ucs4 = parse_string nextf state ucs4

(*
 * [12] PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar -
 *                        "'")* "'"
 * [13] PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9]
 *                        | | [-'()+,./:=?;!*#@$_%]
 *)
let parse_pubid_literal nextf state ucs4 =
  let buf = Buffer.create 30 in
  let rec get_value qt _state ucs4 =
    if ucs4 = qt then
      let value = Buffer.contents buf in
        Buffer.reset buf;
        nextf value
    else if Xmlchar.is_pubid_char ucs4 then (
      fencoder state buf ucs4;
      next_char (get_value qt)
    )
    else
      raise (LexerError "Invalid char in PubidLiteral")
  in
    if ucs4 = Xmlchar.u_apos || ucs4 = Xmlchar.u_quot then
      next_char (get_value ucs4)
    else (
      raise (LexerError "malformed attribute value")
    )

(*
 * [75] ExternalID  ::= 'SYSTEM' S SystemLiteral
 *                      | 'PUBLIC' S PubidLiteral S SystemLiteral
 *)
let parse_external_id nextf state ucs4 =
  get_word
    (fun word state ucs4 ->
       match word with
         | "SYSTEM" ->
             after_blank (parse_system_literal
                            (fun system -> nextf ("", system))
                         ) state (UCS4 ucs4)
         | "PUBLIC" ->
             after_blank
               (parse_pubid_literal
                  (fun public ->
                     Lexer
                       (after_blank
                          (parse_system_literal
                             (fun system -> nextf (public, system))
                          )))
               ) state (UCS4 ucs4)
         | _ ->
             raise (UnknownToken word)
    ) state ucs4

(*                                                      Types]
 * [47] children     ::= (choice | seq) ('?' | '*'
 *                       | '+')?
 * [48] cp            ::= (Name | choice | seq) ('?'
 *                        | '*' | '+')?
 * [49] choice        ::= '(' S? cp ( S? '|' S? cp   [VC: Proper Group/PE
 *                        )+ S? ')'                  Nesting]
 * [50] seq           ::= '(' S? cp ( S? ',' S? cp   [VC: Proper Group/PE
 *                        )* S? ')'                  Nesting]
 *)
let parse_children nextf state ucs4 =
  let rec get_cp_list nextf state ucs4 =
    get_item
      (fun item _state ucs4 ->
         if ucs4 = Xmlchar.u_pipe then
           Lexer (skip_blank (aux_get_cp_list ucs4
                                (fun cps -> nextf (`Choice cps)) [item]))
         else if ucs4 = Xmlchar.u_comma then
           Lexer (skip_blank (aux_get_cp_list ucs4
                                (fun cps -> nextf (`Seq cps)) [item]))
         else if ucs4 = Xmlchar.u_closeparen then
           next_char
             (get_quantify
                (fun q state ucs4 -> nextf (`Seq ([item], q)) state ucs4))
       else
         raise (LexerError "expected '|' or ')'")
      ) state ucs4
  and get_item nextf state ucs4 =
    if ucs4 = Xmlchar.u_openparen then
      get_item
        (fun item _state ucs4 ->
           if ucs4 = Xmlchar.u_pipe then
             Lexer (skip_blank (aux_get_cp_list ucs4
                                  (fun cps -> nextf (`Choice cps)) [item]))
           else if ucs4 = Xmlchar.u_comma then
             Lexer (skip_blank (aux_get_cp_list ucs4
                                  (fun cps -> nextf (`Seq cps)) [item]))
    else if ucs4 = Xmlchar.u_closeparen then
      next_char
        (get_quantify
           (fun q state ucs4 ->
              nextf (`Seq ([item], q)) state ucs4))
    else
      raise (LexerError "expected '|' or ')'")
        ) state ucs4
    else
      parse_name (fun name state ucs4 ->
                    get_quantify (fun q state ucs4 ->
                                    nextf (`Name (name, q)) state ucs4
                                 ) state ucs4) state ucs4
  and aux_get_cp_list sep nextf acc state ucs4 =
    get_item
      (fun item _state ucs4 ->
         if ucs4 = sep then
           Lexer (skip_blank (aux_get_cp_list sep nextf (item :: acc)))
         else if ucs4 = Xmlchar.u_closeparen then
           next_char
             (get_quantify
                (fun q state ucs4 -> nextf ((item :: acc), q) state ucs4))
         else
           raise (LexerError "expected '|' or ')'")
      ) state ucs4
  and get_quantify nextf state ucs4 =
    if ucs4 = Xmlchar.u_quest then
      Lexer (skip_blank (nextf `Quest))
    else if ucs4 = Xmlchar.u_plus then
      Lexer (skip_blank (nextf `Plus))
    else if ucs4 = Xmlchar.u_star then
      Lexer (skip_blank (nextf `Star))
    else
      skip_blank (nextf `One) state (UCS4 ucs4)
  in
    get_cp_list nextf state ucs4

(*
 * [46] contentspec  ::= 'EMPTY' | 'ANY' | Mixed
 *                       | children
 * [51] Mixed        ::= '(' S? '#PCDATA' (S? '|' S?
 *                       Name)* S? ')*'
 *                       | '(' S? '#PCDATA' S? ')'     [VC: Proper Group/PE
 *                                                     Nesting]
 *                                                     [VC: No Duplicate
 *)
let parse_contentspec nextf state ucs4 =
  let rec get_list nextf acc _state ucs4 =
    if ucs4 = Xmlchar.u_pipe then
      Lexer (skip_blank
               (parse_name
                  (fun name state ucs4 ->
                     skip_blank (get_list nextf (name :: acc)) state (UCS4 ucs4)
                  )))
    else if ucs4 = Xmlchar.u_closeparen then
      if List.length acc = 1 then
        next_char (fun state ucs4 ->
                     if ucs4 = Xmlchar.u_star then
                       next_char (nextf acc)
                     else
                       nextf acc state ucs4
                  )
      else
        next_char  (expected_char Xmlchar.u_star (next_char (nextf acc)))
    else
      raise (LexerError "expected '|' or ')'")
  in
  let get_mixed nextf state ucs4 =
    expected_char Xmlchar.u_openparen
      (Lexer (skip_blank
                (fun state ucs4 ->
                   if ucs4 = Xmlchar.u_sharp then
                     next_char
                       (get_word
                          (fun word state ucs4 ->
                             match word with
                               | "PCDATA" ->
                                   get_list
                                     (fun mixed ->
                                        nextf (`Mixed mixed))
                                     [] state ucs4
                               | _ ->
                                   raise (LexerError
                                            "expected #PCDATA")
                          ))
                   else
                     parse_children
                       (fun childs state ucs4 ->
                          nextf (`Children childs) state ucs4) state ucs4
                )
             )) state ucs4
  in
    get_word
      (fun word state ucs4 ->
         match word with
           | "EMPTY" ->
               skip_blank (expected_char Xmlchar.u_gt (nextf `EMPTY)
                          ) state (UCS4 ucs4)
           | "ANY" ->
               skip_blank (expected_char Xmlchar.u_gt (nextf `ANY)
                          ) state (UCS4 ucs4)
    | "" ->
        get_mixed (fun mixed state ucs4 ->
                     skip_blank
                       (expected_char Xmlchar.u_gt (nextf mixed)
                       ) state (UCS4 ucs4)
                  ) state ucs4
    | _ ->
        raise (LexerError "invalid syntax of elementdecl")
      ) state ucs4

(*
 * [45] elementdecl  ::= '<!ELEMENT' S Name S      [VC: Unique Element
 *                       contentspec S? '>'        Type Declaration]
 *)
let parse_elementdecl nextf state ucs4 =
  parse_name
    (fun name state ucs4 ->
       after_blank
         (parse_contentspec
            (fun contentspec -> nextf (`Elementdecl (name, contentspec)))
         ) state (UCS4 ucs4)
    ) state ucs4

(*
 * [53] AttDef        ::= S Name S AttType S DefaultDecl
 * [54] AttType       ::= StringType |
 *                        TokenizedType |
 *                        EnumeratedType
 * [55] StringType    ::= 'CDATA'
 * [56] TokenizedType ::= 'ID'                      [VC: ID]
 *                                                  [VC: One ID per
 *                                                  Element Type]
 *                                                  [VC: ID
 *                                                  Attribute
 *                                                  Default]
 *                        | 'IDREF'                 [VC: IDREF]
 *                        | 'IDREFS'                [VC: IDREF]
 *                        | 'ENTITY'                [VC: Entity
 *                                                  Name]
 *                        | 'ENTITIES'              [VC: Entity
 *                                                  Name]
 *                        | 'NMTOKEN'               [VC: Name Token]
 *                        | 'NMTOKENS'              [VC: Name Token]
 * [60] DefaultDecl   ::= '#REQUIRED'
 *                        | '#IMPLIED'
 *                        | (('#FIXED' S)? [VC: Required Attribute]
 *                        AttValue)
 *                                                  [VC: Attribute Default
 *                                                  Value Syntactically
 *                                                  Correct]
 *                                                  [WFC: No < in Attribute
 *                                                  Values]
 *                                                  [VC: Fixed Attribute
 *                                                  Default]
 *                                                  [WFC: No External Entity
 *                                                  References]
 *)
let parse_attdefs nextf attname state ucs4 =
  let rec get_attdefs acc state ucs4 =
    if ucs4 = Xmlchar.u_gt then
      nextf (`AttlistDecl (attname, List.rev acc))
    else
      parse_name
        (parse_atttype
           (fun name atttype state ucs4 ->
              after_blank (parse_defaultdecl
                             (fun decl state ucs4 ->
                                if ucs4 = Xmlchar.u_gt then
                                  get_attdefs (decl :: acc) state ucs4
                                else if Xmlchar.is_space ucs4 then
                                  Lexer (skip_blank (get_attdefs (decl :: acc)))
                                else
                                  raise (LexerError "expected space or '>'")
                             )
                             name atttype
                          ) state (UCS4 ucs4)
           )) state ucs4
  and parse_atttype nextf name state ucs4 =
    after_blank
      (get_word
         (fun word state ucs4 ->
            match word with
              | "CDATA" -> nextf name `CDATA state ucs4
              | "ID" -> nextf name `ID state ucs4
              | "IDREF" -> nextf name `IDREF state ucs4
              | "IDREFS" -> nextf name `IDREFS state ucs4
              | "ENTITY" -> nextf name `ENTITY state ucs4
              | "ENTITIES" -> nextf name `ENTITIES state ucs4
              | "NMTOKEN" -> nextf name `NMTOKEN state ucs4
              | "NMTOKENS" -> nextf name `NMTOKENS state ucs4
              | "NOTATION" ->
                  get_notation (nextf name) state ucs4
              | "" ->
                  if ucs4 = Xmlchar.u_openparen then
                    Lexer (skip_blank
                             (get_list parse_nmtoken
                                (fun nmtokens ->
                                   next_char
                                     (nextf name
                                        (`ENUMERATION nmtokens)))
                                []))
                  else
          raise (LexerError "expected attribyte type")
              | _ ->
                  raise (LexerError "invalid syntax")
         )
      ) state (UCS4 ucs4)
  and get_notation nextf state ucs4 =
    after_blank
      (expected_char Xmlchar.u_openparen
         (Lexer (skip_blank
                   (get_list parse_name
                      (fun names ->
                         next_char (nextf (`NOTATION names)))
                      [])))
      ) state (UCS4 ucs4)
  and get_list f (nextf: string list -> lstream) acc state ucs4  =
    f (fun name state ucs4 ->
         skip_blank (fun _state ucs4 ->
                       if ucs4 = Xmlchar.u_pipe then
                         Lexer (skip_blank (get_list f nextf (name :: acc)))
                       else if ucs4 = Xmlchar.u_closeparen then
                         nextf (List.rev acc)
                       else
                         raise (LexerError "expected '|' or ')'")
                    ) state (UCS4 ucs4)
      ) state ucs4
  and parse_defaultdecl nextf name atttype state ucs4 =
    if ucs4 = Xmlchar.u_sharp then
      next_char
        (get_word
           (fun word state ucs4 ->
              match word with
                | "REQUIRED" ->
                    nextf (name, atttype, `REQUIRED) state ucs4
                | "IMPLIED" ->
                    nextf (name, atttype, `IMPLIED) state ucs4
                | "FIXED" ->
                    after_blank
                      (parse_attvalue
                         (fun value ->
                            nextf (name, atttype, (`FIXED value))
                         )) state (UCS4 ucs4)
                | _ ->
                    raise (LexerError "unexpected defaultdecl value")
           ))
    else
      parse_attvalue (fun value -> nextf (name, atttype, `Default value))
        state ucs4
  in
    if Xmlchar.is_space ucs4 then
      Lexer (skip_blank (get_attdefs []))
    else if Xmlchar.u_gt = ucs4 then
      nextf (`AttlistDecl (attname, []))
    else
      raise (LexerError "unexpected char")
        

(*
 * [52] AttlistDecl   ::= '<!ATTLIST' S Name AttDef* S? '>'
 *)
let parse_attlistdecl nextf state ucs4 =
  parse_name (parse_attdefs nextf) state ucs4

(*
 * [70] EntityDecl    ::= GEDecl | PEDecl
 * [71] GEDecl        ::= '<!ENTITY' S Name S EntityDef S? '>'
 * [72] PEDecl       ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
 * [73] EntityDef    ::= EntityValue | (ExternalID NDataDecl?)
 * [74] PEDef        ::= EntityValue | ExternalID
 * [76] NDataDecl    ::= S 'NDATA' S Name                [VC: Notation
 *)
let parse_entitydecl nextf state ucs4 =
  let get_end ent state ucs4 =
    skip_blank (expected_char Xmlchar.u_gt (nextf (`EntityDecl ent)))
      state (UCS4 ucs4)
  in
  let parse_ndata name ext_id nextf state ucs4 =
    if Xmlchar.is_space ucs4 then
      Lexer (
        skip_blank
          (get_word
             (fun word state ucs4 ->
                match word with
                  | "NDATA" ->
                      after_blank
                        (parse_name
                           (fun ndataname state ucs4 ->
                              nextf (`Entity (name,
                                              (`UnparsedExternalID
                                                 (ext_id, ndataname))))
                                state ucs4)
                        ) state (UCS4 ucs4)
                  | "" ->
                      nextf (`Entity (name, `ExternalID ext_id))
                        state ucs4
                  | _ ->
                      raise (LexerError "expecred NDATA")
             )))
    else
      nextf (`Entity (name, `ExternalID ext_id)) state ucs4
  in
    if ucs4 = Xmlchar.u_percent then
      Lexer (after_blank
               (parse_name
                  (fun name state ucs4 ->
                     after_blank
                       (fun state ucs4 ->
                          if ucs4 = Xmlchar.u_apos ||
                            ucs4 = Xmlchar.u_quot then
                              parse_entity_value
                                (fun ent ->
                                   next_char
                                     (get_end
                                        (`ParameterEntity
                                           (name,
                                            (`EntityValue ent)))
                                     )
                                ) state ucs4
                          else
                            parse_external_id
                              (fun ext_id ->
                                 next_char
                                   (get_end (`ParameterEntity
                                               (name,
                                                (`ExternalID ext_id)))
                                   )
                              ) state ucs4
                       ) state (UCS4 ucs4)
                  )))
    else
      parse_name
        (fun name state ucs4 ->
           after_blank
             (fun state ucs4 ->
                if ucs4 = Xmlchar.u_apos ||
                  ucs4 = Xmlchar.u_quot then
                    parse_entity_value
                      (fun ent ->
                         next_char
                           (get_end
                              (`Entity (name, (`EntityValue ent))))
                      ) state ucs4
                else
                  parse_external_id
                    (fun ext_id ->
                       next_char (parse_ndata name ext_id get_end)
                    ) state ucs4
             ) state (UCS4 ucs4)
        ) state ucs4

(*
 * [82] NotationDecl ::= '<!NOTATION' S Name S        [VC: Unique
 *                       (ExternalID | PublicID) S?   Notation Name]
 *                       '>'
 * [83] PublicID     ::= 'PUBLIC' S PubidLiteral
 *)
let parse_notationdecl nextf state ucs4 =
  let rec get_external_id nextf state ucs4 =
    get_word
      (fun word state ucs4 ->
         match word with
           | "SYSTEM" ->
               after_blank
                 (parse_system_literal
                    (fun system -> Lexer (skip_blank (nextf ("", system))))
                 ) state (UCS4 ucs4)
           | "PUBLIC" ->
               after_blank (get_public_id nextf) state (UCS4  ucs4)
           | _ ->
               raise (LexerError "invalid syntax")
      ) state ucs4
  and get_public_id nextf state ucs4 =
    parse_pubid_literal
      (fun pubid ->
         next_char
           (fun state ucs4 ->
              if ucs4 = Xmlchar.u_gt then
                nextf (pubid, "") state ucs4
              else if Xmlchar.is_space ucs4 then
                Lexer
                  (skip_blank
                     (fun state ucs4 ->
                        if ucs4 = Xmlchar.u_gt then
                          nextf (pubid, "") state ucs4
                        else
                          parse_system_literal
                            (fun system ->
                               Lexer (skip_blank
                                        (nextf (pubid, system)))
                            ) state ucs4
                     )
                  )
              else
                raise (LexerError "invalid syntax")
           )
      ) state ucs4
  in
    parse_name (fun name state ucs4 ->
                  after_blank (get_external_id
                                 (fun ext_id _state ucs4 ->
                                    if ucs4 = Xmlchar.u_gt then
                                      nextf (`NotationDecl (name, ext_id))
                                    else
                                      raise (LexerError "invalid syntax")
                                 )
                              ) state (UCS4 ucs4)
               )
      state ucs4
      
(*
 * [28b] intSubset   ::= (markupdecl | DeclSep)*
 * [28a] DeclSep     ::= PEReference | S           [WFC: PE Between
 *                                                 Declarations]
 * [29] markupdecl   ::= elementdecl | AttlistDecl [VC: Proper
 *                       | EntityDecl |            Declaration/PE
 *                       NotationDecl | PI |       Nesting]
 *                       Comment
 *                                                 [WFC: PEs in
 *                                                 Internal Subset]
 * [77] TextDecl     ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
 * [78] extParsedEnt ::= TextDecl? content
 *)
let parse_intsubset mark_end nextf state ucs4 =
  let rec get_list acc _state data =
    if data = mark_end then
      nextf (List.rev acc)
    else match data with
      | UCS4 ucs4 ->
          if ucs4 = Xmlchar.u_lt then
            next_char
              (fun state ucs4 ->
                 if ucs4 = Xmlchar.u_excl then
                   next_char  (parse_markupdecl_with_excl (go_list acc))
                 else if ucs4 = Xmlchar.u_quest then
                   next_char
                     (parse_pi
                        (fun target data ->
                           (go_list acc
                              (`PI (target, fencoder_string state data)))))
                 else
                   raise (LexerError "expected '!' or '?'")
              )
          else if ucs4 = Xmlchar.u_percent then
            next_char (parse_name (fun name _state ucs4 ->
                                     if ucs4 = Xmlchar.u_semicolon then
                                       go_list acc (`DeclSect name)
                                     else                            
                                       raise (LexerError "expected ';'")
                                  ))
          else
            raise (LexerError "expected '<' or ']'")
      | EOB ->
          raise (LexerError "expected '<' or ']'")
  and parse_markupdecl_with_excl nextf state ucs4 =
    get_word (fun word state ucs4 ->
                match word with
                  | "ELEMENT" ->
                      after_blank (parse_elementdecl nextf) state (UCS4 ucs4)
                  | "ATTLIST" ->
                      after_blank (parse_attlistdecl nextf) state (UCS4 ucs4)
                  | "ENTITY" ->
                      after_blank (parse_entitydecl nextf) state (UCS4 ucs4)
                  | "NOTATION" ->
                      after_blank (parse_notationdecl nextf) state (UCS4 ucs4)
                  | "" ->
                      if ucs4 = Xmlchar.u_dash then
                        next_char
                          (parse_comment
                             (fun comment -> nextf (`Comment comment)))
                      else
                        raise (LexerError "invalid syntax")
                  | _ -> raise (UnknownToken word)
             ) state ucs4
  and go_list acc elt =
    let rec aux_go_list acc state data =
      match data with
        | UCS4 ucs4 ->
            if Xmlchar.is_space ucs4 then
              Lexer (aux_go_list acc)
            else
              get_list acc state data
        | EOB ->
            Lexer (aux_go_list acc)
    in
      Lexer (aux_go_list (elt :: acc))
  in
    get_list [] state (UCS4 ucs4)

(*
 * [28] doctypedecl  ::= '<!DOCTYPE' S Name (S     [VC: Root Element
 *                       ExternalID)? S? ('['      Type]
 *                       intSubset ']' S?)? '>'
 *                                                 [WFC: External
 *                                                 Subset]
 *)
let parse_doctype nextf state ucs4 =
  let dtd_name f = after_blank (parse_name f) in
  let dtd_external_id f name _state ucs4 =
    if ucs4 = Xmlchar.u_gt then
      nextf {dtd_name = name; dtd_external_id = None; dtd_intsubset = []}
    else if Xmlchar.is_space ucs4 then
      Lexer (skip_blank
               (fun state ucs4 ->
                  if ucs4 = Xmlchar.u_openbr ||
                    ucs4 = Xmlchar.u_gt then
                      f name None state ucs4
                  else
                    parse_external_id
                      (fun ext_id ->
                         Lexer (skip_blank
                                  (f name (Some ext_id)))) state ucs4
               ))
    else
      raise (LexerError "expected space or '>'")
  in
  let dtd_intsubset name ext_id _state ucs4 =
    if ucs4 = Xmlchar.u_openbr then
      Lexer (skip_blank
               (parse_intsubset (UCS4 Xmlchar.u_closebr)
                  (fun intsubset ->
                     Lexer (skip_blank
                              (expected_char Xmlchar.u_gt
                                 (nextf { dtd_name = name;
                                          dtd_external_id = ext_id;
                                          dtd_intsubset = intsubset}
                                 ))))))
    else if ucs4 = Xmlchar.u_gt then
      nextf { dtd_name = name; dtd_external_id = ext_id; dtd_intsubset = []}
    else
      raise (LexerError "bad doctype syntax")
  in
    dtd_name (dtd_external_id dtd_intsubset) state (UCS4 ucs4)

(*
 * [25] Eq          ::= S? '=' S?
 *)
let parse_eq nextf state ucs4 =
  skip_blank (expected_char Xmlchar.u_eq (Lexer (skip_blank nextf))
             ) state (UCS4 ucs4)

(*
 * [41] Attribute   ::= Name Eq AttValue
 * [25] Eq          ::= S? '=' S?
 * [10] AttValue    ::= '"' ([^<&"] | Reference)* '"'
 *                      | "'" ([^<&'] | Reference)* "'"
 *)
let rec parse_attributes tag attrs nextf state ucs4 =
  if ucs4 = Xmlchar.u_gt then
    nextf tag attrs true
  else if ucs4 = Xmlchar.u_slash then
    next_char (expected_char Xmlchar.u_gt (nextf tag attrs false))
  else
    let smth state ucs4 =
      if ucs4 = Xmlchar.u_gt then
        nextf tag attrs true
      else if ucs4 = Xmlchar.u_slash then
        next_char (expected_char Xmlchar.u_gt (nextf tag attrs false))
      else
        parse_name
          (fun name ->
             (parse_eq (parse_attvalue
                          (fun value ->
                             parse_attributes tag
                               ((name, value) :: attrs) nextf
                          )))
          ) state ucs4
    in
      after_blank smth state (UCS4 ucs4)

(*
 * [40] STag      ::= '<' Name (S Attribute)* S? '>'
 *)
let parse_start_element nextf state ucs4 =
  parse_name (fun name -> parse_attributes name [] nextf) state ucs4

(*
 * [42] ETag ::= '</' Name S? '>
 *)
let parse_end_element nextf state ucs4 =
  parse_name
    (fun name state ucs4 ->
       skip_blank (expected_char Xmlchar.u_gt (nextf name)) state (UCS4 ucs4)
    ) state ucs4

(*
 * [18] CDSect  ::= CDStart CData CDEnd
 * [19] CDStart ::= '<![CDATA['
 * [20] CData   ::= (Char* - (Char* ']]>' Char* ))
 * [21] CDEnd   ::= ']]>'
 *)
let parse_cdata nextf =
  let buf = Buffer.create 30 in
  let rec get_cdata state = function
    | UCS4 ch1 ->
        if ch1 = Xmlchar.u_closebr then
          next_char
            (fun state ch2 ->
               if ch2 = Xmlchar.u_closebr then
                 let rec aux_tail state ucs4 =
                   if ucs4 = Xmlchar.u_gt then
                     let cdata = Buffer.contents buf in
                       Buffer.reset buf;
                       nextf cdata
                   else if ucs4 = Xmlchar.u_closebr then (
                     fencoder state buf Xmlchar.u_closebr;
                     next_char aux_tail
                   )
                   else (
                     fencoder state buf Xmlchar.u_closebr;
                     fencoder state buf Xmlchar.u_closebr;
                     fencoder state buf ucs4;
                     Lexer get_cdata
                   )
                 in
                   next_char aux_tail
               else (
                 fencoder state buf ch1;
                 fencoder state buf ch2;
                 Lexer get_cdata
               ))
        else (
          fencoder state buf ch1;
          Lexer get_cdata
        )
    | EOB ->
        if Buffer.length buf = 0 then
          Lexer get_cdata
        else
          let cdata = Buffer.contents buf in
            Buffer.reset buf;
            Token (Text cdata, Some get_cdata, true)
  in
    next_char (get_word
                 (fun word state ucs4 ->
                    match word with
                      | "CDATA" ->
                          expected_char Xmlchar.u_openbr
                            (next_char
                               (fun state ucs4 -> get_cdata state (UCS4 ucs4)))
                            state ucs4
                      | _ ->
                          raise (UnknownToken word)
                 ))

(*
 * [23] XMLDecl     ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
 * [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
 * [25] Eq          ::= S? '=' S?
 * [26] VersionNum  ::= '1.0'
 *)
let parse_xmldecl state data =
  let ua = Array.of_list data in
  let s i =
    if i < Array.length ua then ua.(i)
    else raise (LexerError ("bad xml declaration: " ^
                              (state.fencoder_error data)))
  in
  let buf = Buffer.create 30 in
  let ascii_letter ucs4 =
    ucs4 >= Xmlchar.of_char 'a' && ucs4 <= Xmlchar.of_char 'z'
  in
  let rec get_name i =
    let ucs4 = s i in
      if ascii_letter ucs4 then (
        fencoder state buf ucs4;
        get_name (i+1)
      )
      else (
        let name = Buffer.contents buf in
          Buffer.reset buf;
          name, i
      )
  in
  let rec get_value qt i =
    let ucs4 = s i in
      if ucs4 = qt then
        let value = Buffer.contents buf in
          Buffer.reset buf;
          value, i
      else (
        fencoder state buf ucs4;
        get_value qt (i+1)
      )
  in
  let rec skip_blank i =
    if i < Array.length ua && Xmlchar.is_space ua.(i) then
      skip_blank (i+1)
    else i
  in
  let rec get_attrs acc i =
    if i < Array.length ua then
      let name, i = get_name i in
      let i = skip_blank i in
        if s i = Xmlchar.u_eq then
          let i = skip_blank (i+1) in
            if s i = Xmlchar.u_apos || s i = Xmlchar.u_quot then
              let value, i = get_value (s i) (i+1) in
                if (i+1) < Array.length ua &&
                  Xmlchar.is_space ua.(i+1) then
                    let i = skip_blank (i+1) in
                      get_attrs ((name, value) :: acc) i
                else
                  ((name, value) :: acc)
            else
              raise (LexerError ("bad xml declaration: " ^
                                   (state.fencoder_error data)))
        else
          raise (LexerError ("bad xml declaration: " ^
                               (state.fencoder_error data)))
    else
      acc
  in
  let attrs = get_attrs [] 0 in
    List.rev attrs

let opened_lt state ucs4 =
  if ucs4 = Xmlchar.u_slash then
    next_char
      (parse_end_element (fun name -> Token (EndElement name, None, true)))
  else if ucs4 = Xmlchar.u_quest then
    next_char
      (parse_pi (fun target data -> Token
                   (Pi (target, fencoder_string state data), None, true)))
  else if ucs4 = Xmlchar.u_excl then
    next_char
      (fun state ucs4 ->
         if ucs4 = Xmlchar.u_openbr then
           parse_cdata (fun cdata -> Token (Text cdata, None, true))
         else if ucs4 = Xmlchar.u_dash then
           next_char
             (parse_comment (fun comment -> Token (Comment comment, None, true)))
         else
           get_word
             (fun word state ucs4 ->
                match word with
                  | "DOCTYPE" ->
                      parse_doctype
                        (fun dtd -> Token ((Doctype dtd), None, true)) state ucs4
                  | _ ->
                      raise (UnknownToken word)
             ) state ucs4
      )
  else
    parse_start_element
      (fun name attrs flag ->
         if not flag then
           Token (StartElement (name, List.rev attrs),
                  Some (fun _state _data ->
                          Token ((EndElement name), None, false)),
                  true)
         else
           Token (StartElement (name, (List.rev attrs)), None, true)
      ) state ucs4

let lexer state ucs4 =
  if ucs4 = Xmlchar.u_lt then
    next_char opened_lt
  else if Xmlchar.is_space ucs4 then
    parse_whitespace state ucs4
  else
    parse_text state ucs4
        
let process_xmldecl encoding_handler attrs state =
  let version =
    try List.assoc "version" attrs with Not_found -> "" in
    if version <> "1.0" then
      raise (LexerError ("unknown version of xml: '" ^ version ^ "'"));
    let encoding = try List.assoc "encoding" attrs with Not_found -> "" in
      if encoding = "" then
        next_char lexer
      else
        let up = String.uppercase encoding in
          if state.encoding = up then
            next_char lexer
          else
            let fdecoder =
              match up with
                | "ASCII" | "US-ASCII" ->
                    Xmlencoding.decode_ascii
                | "LATIN1" | "ISO-8859-1" ->
                    Xmlencoding.decode_latin1
                | "UTF-8" ->
                    Xmlencoding.decode_utf8
                | "UTF-16" | "UTF-16BE" ->
                    Xmlencoding.decode_utf16 BE
                | "UTF-16LE" ->
                    Xmlencoding.decode_utf16 BE
                | "UCS-4" | "UCS-4BE" ->
                    Xmlencoding.decode_ucs4
                | "UCS-4LE" ->
                    Xmlencoding.decode_ucs4le
                | _ ->
                    encoding_handler encoding
            in
              SwitchDecoder (encoding, fdecoder)

(*
 * [22] prolog      ::= XMLDecl? Misc* (doctypedecl Misc* )?
 * [27] Misc        ::= Comment | PI | S
 *)
let start_lexer encoding_handler state ucs4 =
  if ucs4 = Xmlchar.u_lt then
    next_char
      (fun state ucs4 ->
         if ucs4 = Xmlchar.u_quest then
           next_char (parse_pi
                        (fun pi data ->
                           if pi = "xml" then
                             let attrs = parse_xmldecl state data in
                               process_xmldecl encoding_handler attrs state
                           else
                             Token (Pi (pi,
                                        fencoder_string state data),
                                    None, true)))
         else
           opened_lt state ucs4
      )
  else
    lexer state ucs4

let string_of_production = function
  | StartElement (name, _attrs) ->
      Printf.sprintf "StartElement %S" name
  | EndElement name ->
      Printf.sprintf "EndElement %S" name
  | Text text ->
      Printf.sprintf "Text %S" text
  | Whitespace spaces ->
      Printf.sprintf "Whitespace %S" spaces
  | Pi (target, data) ->
      Printf.sprintf "Pi %s %S" target data
  | Comment comment ->
      Printf.sprintf "Comment %S" comment
  | Doctype _ ->
      Printf.sprintf "Doctype"
  | EndOfBuffer ->
      Printf.sprintf "EndOfBuffer"
  | EndOfData ->
      Printf.sprintf "EndOfData"

(* 2.11 End-of-Line Handling
 * \r\n -> \n
 * \r -> \n
 *)
let norm_nl state ucs4 =
  if ucs4 = Xmlchar.u_cr then
    None
  else if ucs4 = Xmlchar.u_lf then
    if state then
      Some ucs4
    else
      Some Xmlchar.u_lf
  else
    if state then
      Some Xmlchar.u_lf
    else
      Some ucs4

let rec fparser norm_nl_state flexer state =
  let rec get_char norm_nl_state j =
    if j < String.length state.buffer then
      match state.fdecoder state.buffer j with
        | Invalid ->
            raise InvalidInput
        | TooFew ->
            if state.finish then
              raise (LexerError "unexpected end of data")
            else
              (norm_nl_state, j, EOB)
        | Shift k ->
            get_char norm_nl_state k
        | Result (k, ucs4) ->
            match norm_nl norm_nl_state ucs4 with
              | None ->
                  get_char true k
              | Some ucs4 ->
                  (false, k, UCS4 ucs4)
    else
      (norm_nl_state, j, EOB)
  in
  let rec aux_fparser norm_nl_state flexer i state =
    let (norm_nl_state, j, ch) = get_char norm_nl_state i in
      match ch with
        | UCS4 ucs4 ->
            let r =
              match flexer with
                | None -> lexer state ucs4
                | Some v -> v state (UCS4 ucs4)
            in (
              match r with
                | Lexer flexer ->
                    aux_fparser norm_nl_state (Some flexer) j state
                | SwitchDecoder (encoding, fdecoder) ->
                    aux_fparser norm_nl_state None j
                      {state with encoding = encoding; fdecoder = fdecoder}
                | Token (tag, flexer, emit) ->
                    let newstate =
                      {state with
                         i = if emit then j else i;
                         fparser = fparser norm_nl_state flexer} in
                      (newstate, tag)
              )
        | EOB ->
            if state.finish then
              match flexer with
                | None ->
                    let newstate = {state with is_parsing = false; i=j} in
                      (newstate, EndOfData)
                | Some v ->
                    match v state EOB with
                      | Lexer _ ->
                          failwith "Unexpected end of data"
                      | Token (tag, flexer, _) ->
                          let newstate =
                            {state with i = j;
                               fparser = fparser norm_nl_state flexer } in
                            (newstate, tag)
                      | SwitchDecoder _ ->
                          let newstate =
                            {state with is_parsing = false; i = j } in
                            (newstate, EndOfData)
            else
              match flexer with
                | None ->
                    let newstate =
                      {state with i = j;
                         fparser = fparser norm_nl_state flexer} in
                      (newstate, EndOfBuffer)
                | Some v ->
                    match v state EOB with
                      | Lexer flexer ->
                          let newstate =
                            {state with i = j;
                               fparser = fparser norm_nl_state (Some flexer)} in
                            (newstate, EndOfBuffer)
                      | SwitchDecoder (encoding, fdecoder) ->
                          let newstate =
                            {state with i = j;
                               encoding = encoding;
                               fdecoder = fdecoder;
                               fparser = fparser norm_nl_state None} in
                            (newstate, EndOfBuffer)
                      | Token (tag, flexer, _emit) ->
                          let newstate =
                            {state with i = j;
                               fparser = fparser norm_nl_state flexer} in
                            (newstate, tag)
  in
    aux_fparser norm_nl_state flexer state.i state
                
let create ?encoding ?unknown_encoding_handler ?entity_resolver () =
  let entity_resolver =
    match entity_resolver with
      | None -> (fun name -> raise (LexerError ("Unknown entity name " ^ name)))
      | Some v -> v
  in
  let encoding_handler =
    match unknown_encoding_handler with
      | None -> (fun _ -> raise (LexerError "Unknown encoding"))
      | Some f -> f
  in
  let fencoder_error uclist =
    let buf = Buffer.create (List.length uclist) in
      List.iter
        (fun ucs4 -> List.iter (Buffer.add_char buf)
           (Xmlencoding.encode_utf8 ucs4)) uclist;
      Buffer.contents buf
  in
    match encoding with
      | None ->
          let fparser =
            fun state ->
              if String.length state.buffer < 4 then
                (state, EndOfBuffer)
              else
                let encoding, fdecoder =
                  Xmlencoding.autodetect_encoding
                    state.buffer.[0] state.buffer.[1]
                    state.buffer.[2] state.buffer.[3] in
                  fparser false
                    (Some (ignore_eob (start_lexer encoding_handler)))
                    {state with encoding = encoding; fdecoder = fdecoder}
          in
            {
              is_parsing = true;
              finish = false;
              i = 0;
              buffer = "";
              entity_resolver = entity_resolver;
              encoding = "";
              fencoder = Xmlencoding.encode_utf8;
              fencoder_error = fencoder_error;
              fdecoder = Xmlencoding.decode_utf8;
              fparser = fparser;
            }
      | Some e ->
          let fdecoder =
            match e with
              | "ASCII" ->
                  decode_ascii
              | "LATIN1" ->
                  decode_latin1
              | "UTF-8" ->
                  decode_utf8
              | "UTF-16" ->
                  decode_utf16 BE
              | "UCS-4" ->
                  decode_ucs4
              | _ ->
                  match unknown_encoding_handler with
                    | None ->
                        failwith "Unknown encoding"
                    | Some f ->
                        f e
          in
            {
              is_parsing = true;
              finish = false;
              i = 0;
              buffer = "";
              entity_resolver = entity_resolver;
              encoding = e;
              fencoder = Xmlencoding.encode_utf8;
              fencoder_error = fencoder_error;
              fdecoder = fdecoder;
              fparser = fparser false
                (Some (ignore_eob (start_lexer encoding_handler)))
            }

let add_buffer state buffer =
  let newbuffer =
    if state.i < String.length state.buffer then
      (if state.i > 0 then
         String.sub state.buffer state.i (String.length state.buffer - state.i)
       else
         state.buffer
      ) ^ buffer
    else
      buffer
  in
    {state with i = 0; buffer = newbuffer}
    
  
let parse ?buf ?finish state =
  let finish =
    match finish with
      | None -> state.finish
      | Some v -> v
  in
  if state.is_parsing then
    let newstate =
      match buf with
        | None -> {state with finish = finish}
        | Some v ->
            let newbuffer =
              if state.i < String.length state.buffer then
                (if state.i > 0 then
                   String.sub state.buffer state.i
                     (String.length state.buffer - state.i)
                 else
                   state.buffer
                ) ^ v
              else
                v
            in
              {state with i = 0; buffer = newbuffer; finish = finish}
    in
      state.fparser newstate
  else
    raise Finished

let set_finish state =
  {state with finish = true}

(*
let parse_dtd str =
  let entity_resolver =
    (fun name -> raise (LexerError ("Unknown entity name " ^ name))) in
  let encoding_handler =
    (fun name -> raise (LexerError ("Unknown encoding " ^ name))) in
  let enc = "UTF-8" in
  let dtd_lexer =
    skip_blank (parse_intsubset EOD
                  (fun intsubset -> Token (Doctype { dtd_name = "";
                                                     dtd_external_id = None;
                                                     dtd_intsubset = intsubset
                                                   }, lexer))) in
  let state =
    {
      is_parsing = true;
      finish = true;
      encoding = enc;
      fdecoder = make_fdecoder enc encoding_handler str;
      fencoder = Xmlencoding.encode_utf8;
      flexer = None;
      i = 0;
      buffer = str;
      entity_resolver = entity_resolver;
      encoding_handler = encoding_handler
    }
  in
    fparser false dtd_lexer 0 state
*)
      
let set_entity_resolver state entity_resolver =
  {state with  entity_resolver = entity_resolver }

let get_rest_buffer state =
  String.sub state.buffer state.i (String.length state.buffer - state.i)
