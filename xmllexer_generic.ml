(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
end

module type ENCODING =
sig
  val encode_unicode : int -> char list
end

module type STREAM =
sig
  include MONAD
  type stream
  type source

  val make_stream : source -> stream
  val set_decoder : string -> stream -> unit
  val next_char : stream -> (unit -> 'b t) -> (int -> 'b t) -> 'b t
  val error : stream -> exn -> 'a t
end

type external_id =
  | SystemID of string
  | PublicID of string * string

type dtd_attr_type = [
  `CDATA
| `ID
| `NMTOKEN
]

type dtd_attr_default = [
  `Required
| `Implied
]

type dtd_gedecl_entity = [
  | `ExternalID of external_id * string option
  | `EntityValue of string
]

type dtd_pedecl_entity = [
  | `ExternalID of external_id
  | `EntityValue of string
]
    
type dtd_entity_type =
  | GEDecl of string * dtd_gedecl_entity
  | PEDecl of string * dtd_pedecl_entity

type dtd_element_type = [
  `Empty
| `Any
]

type dtd =
  | DTD_PI of string * string
  | DTD_PEReference of string
  | DTD_ATTLIST of string * (string * dtd_attr_type * dtd_attr_default) list
  | DTD_Entity of dtd_entity_type
  | DTD_Element of string * dtd_element_type

type doctype = {
  dtd_name : string;
  dtd_external_id : external_id option;
  dtd : dtd list
}

module type XMLTOKEN =
sig
  type 'a t
  type token

  val emit_pi : string -> string -> token t
  val emit_start_tag : string -> (string * string) list -> bool -> token t
  val emit_end_tag : string -> token t
  val emit_doctype : doctype -> token t
  val emit_text : string -> token t
  val emit_eof : unit -> token t
end
  
let u_nl = 0x000A
let u_cr = 0x000D
  
let u_space = 0x0020
let u_excl = 0x0021
let u_quot = 0x0022
let u_sharp = 0x0023
let u_dollar = 0x0024
let u_percent = 0x0025
let u_amp = 0x0026
let u_apos = 0x0027
let u_lparen = 0x0028
let u_rparen = 0x0029
let u_star = 0x002a
let u_plus = 0x002b
let u_comma = 0x002c
let u_dash = 0x002d
let u_dot = 0x002e
let u_slash = 0x002f
  
let u_1 = 0x0031
let u_9 = 0x0039
let u_colon = 0x003a
let u_semicolon = 0x003b
let u_lt = 0x003c
let u_eq = 0x003d
let u_gt = 0x003e
let u_qmark = 0x003f
    
let u_lbracket = 0x005b
let u_rbracket = 0x005d
let u_underline = 0x005f
  
let u_x = 0x0078
let u_bom = 0xFEFF

let u_at = 0x040
let u_A = 0x0041
let u_B = 0x0042
let u_C = 0x0043
let u_D = 0X0044
let u_E = 0x0045
let u_F = 0x0046
let u_I = 0x0049
let u_K = 0x004b
let u_L = 0x004c
let u_M = 0x004d
let u_N = 0x004e
let u_O = 0x004f
let u_P = 0x0050
let u_Q = 0x0051
let u_R = 0x0052
let u_S = 0x0053
let u_T = 0x0054
let u_U = 0x0055
let u_Y = 0x0059
    
let u_a = 0x0061
let u_e = 0x0065
let u_g = 0x0067
let u_i = 0x0069
let u_l = 0x006c
let u_m = 0x006d
let u_n = 0x006e
let u_o = 0x006f
let u_p = 0x0070
let u_q = 0x0071
let u_r = 0x0072
let u_s = 0x0073
let u_t = 0x0074
let u_u = 0x0075
let u_v = 0x0076

let char_range c r1 r2 = c >= r1 && c <= r2
let one_of u chars = List.mem u chars

let is_space u =
  u = 0x0020 || u = 0x000A || u = 0x0009 || u = 0x000D

module type XNAME =
sig
  val is_name_start_char : int -> bool
  val is_name_char : int -> bool
end

module XName =
struct
  let is_name_start_char u =
    char_range u 0x0061 0x007A || u = u_colon || u = u_underline ||
    char_range u 0x0041 0x005A || char_range u 0xC0 0xD6 ||
    char_range u 0xD8 0xF6 || char_range u 0xF8 0x2FF ||
    char_range u 0x370 0x37D || char_range u 0x37F 0x1FFF ||
    char_range u 0x200C 0x200D || char_range u 0x2070 0x218F ||
    char_range u 0x2C00 0x2FEF || char_range u 0x3001 0xD7FF ||
    char_range u 0xF900 0xFDCF || char_range u 0xFDF0 0xFFFD ||
    char_range u 0x10000 0xEFFFF
    
  let is_name_char u =
    is_name_start_char u || u = u_dash || u = u_dot ||
    char_range u 0x0030 0x0039 || char_range u 0x0300 0x036F ||
    char_range u 0x203F 0x2040
end
  
module Make
  (S : STREAM)
  (E : ENCODING)
  (X : XMLTOKEN with type 'a t = 'a S.t) =
struct
  module E = E
  module S = S
  module X = X
  open S

  type next_state =
    | PrologXmlDeclState
    | PrologMiscState
    | TextState
    | LessThanSignState
    | AfterElement
        
  type state = {
    tmp_buffer : Buffer.t;
    stack : string Stack.t;
    mutable next_state : next_state
  }

  exception Exn_msg of string
  exception Exn_EOF
  exception Exn_ExpectedChar of char list
  exception Exn_ExpectedSpace
  exception Exn_CharToken of int
  exception Error_XMLDecl

  let not_eof () =
    S.fail Exn_EOF

  let rec add_chars state = function
    | [] -> ()
    | x :: xs -> Buffer.add_char state.tmp_buffer x; add_chars state xs

  let extract_buffer state =
    let value = Buffer.contents state.tmp_buffer in
      Buffer.reset state.tmp_buffer;
      value

  let rec consume_sequence strm = function
    | [] -> S.return ()
    | x :: xs ->
      next_char strm not_eof (fun u ->
        if u = x then (
          consume_sequence strm xs
        ) else
          error strm (Exn_CharToken u)
      )
        
  and consume_space strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        S.return ()
      else
        error strm Exn_ExpectedSpace
    )

  let parse_xmldecl data =
    let len = String.length data in
    let i = ref 0 in
    let rec skip_space () =
      if !i < len && (data.[!i] = ' ' || data.[!i] = '\n' ||
          data.[!i] = '\t' || data.[!i] = '\r') then (
        incr i;
        skip_space ()
      ) else
        ()
    in
    let get_lim () =
      if !i < len && data.[!i] = '\"' || data.[!i] = '\'' then
        let lim = data.[!i] in
          incr i;
          lim
      else
        raise Error_XMLDecl
    in
    let rec seq = function
      | [] -> ()
      | x :: xs ->
        if !i < len && data.[!i] = x then (
          incr i;
          seq xs
        )
        else
          raise Error_XMLDecl
    in
    let get_digits () =
      let s = !i in
      let rec aux_loop () =
        if !i < len && data.[!i] >= '0' && data.[!i] <= '9' then (
          incr i;
          aux_loop ()
        ) else
          if !i > s then
            String.sub data s (!i - s)
          else
            raise Error_XMLDecl
      in
        seq ['1';'.'];
        aux_loop ()
    in
    let get_encoding () =
      let s = !i in
      let () =
        if !i < len then
          match data.[!i] with
            | 'a' .. 'z' | 'A' .. 'Z' -> incr i;
            | _ -> raise Error_XMLDecl
        else
          raise Error_XMLDecl
      in
      let rec aux_loop () =
        if !i < len then
          match data.[!i] with
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' ->
              incr i;
              aux_loop ()
            | _ ->
              if s < !i then
                String.sub data s (!i - s)
              else
                raise Error_XMLDecl
        else
          raise Error_XMLDecl
      in
        aux_loop ()
    in
    let get_standalone () =
      if !i < len then
        if data.[!i]  = 'y' then (
          incr i;
          seq ['e';'s'];
          "yes"
        ) else if data.[!i] = 'n' then (
          incr i;
          seq ['o'];
          "no"
        ) else
            raise Error_XMLDecl
        else
          raise Error_XMLDecl
    in            
      skip_space ();
      seq ['v';'e';'r';'s';'i';'o';'n'];
      skip_space ();
      seq ['='];
      skip_space ();
      let lim = get_lim () in
      let version = get_digits () in
        seq [lim];
        skip_space ();
        if !i = len then
          (version, None, None)
        else if !i < len && data.[!i] = 'e' then (
          incr i;
          seq ['n';'c';'o';'d';'i';'n';'g'];
          skip_space ();
          seq ['='];
          skip_space ();
          let lim = get_lim () in
          let encoding = get_encoding () in
            seq [lim];
            skip_space ();
            if !i = len then
              (version, Some encoding, None)
            else if !i < len && data.[!i] = 's' then (
              seq ['t';'a';'n';'d';'a';'l';'o';'n';'e'];
              skip_space ();
              let lim = get_lim () in
              let standalone = get_standalone () in
                seq [lim];
                skip_space ();
                if !i = len then
                  (version, Some encoding, Some standalone)
                else
                  raise Error_XMLDecl
            ) else
              raise Error_XMLDecl
        ) else if !i < len && data.[!i] = 's' then (
          seq ['t';'a';'n';'d';'a';'l';'o';'n';'e'];
          skip_space ();
          let lim = get_lim () in
          let standalone = get_standalone () in
            seq [lim];
            skip_space ();
            if !i = len then
              (version, None, Some standalone)
            else
              raise Error_XMLDecl
        ) else
            raise Error_XMLDecl
            

  type d =
    | N of (int * d) list
    | L of char list

  let entities = N [
    u_a, N [u_m, N [u_p, N [u_semicolon, L ['&']]];
            u_p, N [u_o, N [u_s, N [u_semicolon, L ['\'']]]]];
    u_g, N [u_t, N [u_semicolon, L ['>']]];
    u_l, N [u_t, N [u_semicolon, L ['<']]];
    u_q, N [u_u, N [u_o, N [u_t, N [u_semicolon, L ['"']]]]];
  ]

  let character_reference strm =
    let rec aux_entity u = function
      | N [] -> error strm (Exn_CharToken u)
      | N ((c, L z) :: rest) ->
        if u = c then
          S.return u
        else
          aux_entity u (N rest)
      | N ((c, t) :: rest) ->
           if u = c then
             next_char strm not_eof (fun u ->
               aux_entity u t
             )
           else
             aux_entity u (N rest)
      | L c ->
        S.return u
    in
      next_char strm not_eof (fun u ->
        if u = u_sharp then (
          next_char strm not_eof (fun u ->
            if u = u_x then (
              let rec get_hex_code consumed acc =
                next_char strm not_eof (fun u ->
                  if char_range u 0x0030 0x0039 then
                    get_hex_code true (acc * 16 + (u - 0x0030))
                  else if char_range u 0x0041 0x0046 then
                    get_hex_code true (acc * 16 + (u - 0x0037))
                  else if char_range u 0x0061 0x0066 then
                    get_hex_code true (acc * 16 + (u - 0x0057))
                  else if u = u_semicolon then
                    if consumed then
                      (* test unicode char *)
                      S.return acc
                    else
                      error strm (Exn_msg "Expected hex")
                  else
                    error strm (Exn_CharToken u)
                )
              in
                get_hex_code false 0
            )
            else if char_range u 0x0030 0x0039 then
              let rec get_code consumed acc =
                next_char strm not_eof (fun u ->
                  if char_range u 0x0030 0x0039 then
                    get_code true (acc * 10 + (u - 0x0030))
                  else if u = u_semicolon then
                    if consumed then
                      (* test unicode char *)
                      S.return acc
                    else
                      error strm (Exn_CharToken u)
                  else
                    error strm (Exn_CharToken u)
                )
              in
                get_code false (u - 0x0030)
            else 
              error strm (Exn_CharToken u)
          )
        ) else
          aux_entity u entities
      )

  let rec text_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_lt then
        let txt = extract_buffer state in
          state.next_state <- LessThanSignState;
          X.emit_text txt
      else if u = u_amp then
        character_reference strm >>= fun u ->
          add_chars state (E.encode_unicode u);
          text_state state strm
      else (
        add_chars state (E.encode_unicode u);
        text_state state strm
      )
    )
        

  let comment_state state strm =
    let rec start state strm =
      next_char strm not_eof (fun u ->
        if u = u_dash then
          comment_dash_state state strm
        else
          (* if need, add to state.tmp_buffer, but here we ignore comments *)
          start state strm
      )
    and comment_dash_state state strm =
      next_char strm not_eof (fun u ->
        if u = u_dash then
          comment_dash_dash_state state strm
        else (
          (* if need, add to state.tmp_buffer, but here we ignore comments *)
          start state strm
        )
      )
    and comment_dash_dash_state state strm =
      next_char strm not_eof (fun u ->
        if u = u_gt then
          (* end of comment *)
          S.return ()
        else
          error strm (Exn_msg "Unexpected '--'")
      )

    in
      start state strm


  let rec start_tag_name_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then (
        let tagname = extract_buffer state in
          before_attribute_state tagname [] state strm
      ) else if u = u_slash then (
        let tagname = extract_buffer state in
          self_closing_start_tag_state tagname state strm
      ) else if u = u_gt then (
        let tagname = extract_buffer state in
          Stack.push tagname state.stack;
          state.next_state <- TextState;
          X.emit_start_tag tagname [] false
      ) else if XName.is_name_char u then (
        add_chars state (E.encode_unicode u);
        start_tag_name_state state strm
      ) else
          error strm (Exn_CharToken u)
    )
  and self_closing_start_tag_state tagname state strm =
    next_char strm not_eof (fun u ->
      if u = u_gt then (
        if Stack.is_empty state.stack then
          state.next_state <- AfterElement
        else
          state.next_state <- TextState;
        X.emit_start_tag tagname [] true
      ) else
        error strm (Exn_CharToken u)
    )
  and end_tag_start_state state strm =
    next_char strm not_eof (fun u ->
      if XName.is_name_start_char u then (
        add_chars state (E.encode_unicode u);
        end_tag_state state strm
      ) else
        error strm (Exn_CharToken u)
    )

  and end_tag_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then (
        after_end_tag_state state strm
      ) else if u = u_gt then (
        let tagname = extract_buffer state in
        let name = Stack.pop state.stack in
          if name = tagname then (
            if Stack.is_empty state.stack then
              state.next_state <- AfterElement
            else
              state.next_state <- TextState;
            X.emit_end_tag tagname
          ) else
            error strm (Exn_msg "Invalid end tag name")
      ) else if XName.is_name_char u then (
        add_chars state (E.encode_unicode u);
        end_tag_state state strm
      ) else
          error strm (Exn_CharToken u)
    )

  and after_end_tag_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        after_end_tag_state state strm
      else if u = u_gt then
        let tagname = extract_buffer state in
        let name = Stack.pop state.stack in
          if tagname = name then (
            if Stack.is_empty state.stack then
              state.next_state <- AfterElement
            else
              state.next_state <- TextState;
            X.emit_end_tag tagname
          ) else
            error strm (Exn_msg "Invalid end tag name")
      else
        error strm (Exn_CharToken u)
    )
        
  and before_attribute_state tagname attrs state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        before_attribute_state tagname attrs state strm
      else if u = u_slash then (
        consume_sequence strm [u_gt] >>= fun () ->
        state.next_state <- TextState;
        X.emit_start_tag tagname attrs true
      ) else if u = u_gt then (
        Stack.push tagname state.stack;
        state.next_state <- TextState;
        X.emit_start_tag tagname attrs false
      ) else if XName.is_name_start_char u then (
        add_chars state (E.encode_unicode u);
        attribute_name_state tagname attrs state strm
      ) else
          error strm (Exn_CharToken u)
    )

  and attribute_name_state tagname attrs state strm =
    next_char strm not_eof (fun u ->
      if u = u_eq then (
        let name = extract_buffer state in
          before_attribute_value_state state strm >>= fun value ->
        after_attribute_value_state tagname ((name, value) :: attrs) state strm
      ) else if is_space u then (
        let name = extract_buffer state in
          after_attribute_name_state state strm >>= fun value ->
        after_attribute_value_state tagname ((name, value) :: attrs) state strm
      ) else if XName.is_name_char u then (
        add_chars state (E.encode_unicode u);
        attribute_name_state tagname attrs state strm
      ) else
          error strm (Exn_CharToken u)
    )

  and after_attribute_name_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        after_attribute_name_state state strm
      else if u = u_eq then
        before_attribute_value_state state strm
      else
        error strm (Exn_CharToken u)
    )
      
  and before_attribute_value_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        before_attribute_value_state state strm
      else if u = u_quot then
        attribute_value_double_quoted_state state strm
      else if u = u_apos then
        attribute_value_single_quoted_state state strm
      else
        error strm (Exn_CharToken u)
    )
      
  and attribute_value_double_quoted_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_quot then
        let value = extract_buffer state in
          S.return value
      else if u = u_amp then (
        character_reference strm >>= fun u ->
        add_chars state (E.encode_unicode u);
        attribute_value_double_quoted_state state strm
      ) else if u = u_lt then
        error strm (Exn_CharToken u)
        else (
          add_chars state (E.encode_unicode u);
          attribute_value_double_quoted_state state strm
        )
    )

  and attribute_value_single_quoted_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_apos then
        let value = extract_buffer state in
          S.return value
      else if u = u_amp then (
        character_reference strm >>= fun u ->
        add_chars state (E.encode_unicode u);
        attribute_value_single_quoted_state state strm
      ) else if u = u_lt then
        error strm (Exn_CharToken u)
        else (
          add_chars state (E.encode_unicode u);
          attribute_value_single_quoted_state state strm

        )
    )

  and after_attribute_value_state tagname attrs state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        before_attribute_state tagname attrs state strm
      else if u = u_slash then (
        consume_sequence strm [u_gt] >>= fun () ->
        if Stack.is_empty state.stack then
          state.next_state <- AfterElement
        else
          state.next_state <- TextState;
        X.emit_start_tag tagname attrs true
      ) else if u = u_gt then (
        Stack.push tagname state.stack;
        state.next_state <- TextState;
        X.emit_start_tag tagname attrs false
      ) else
          error strm (Exn_CharToken u)
    )
          
      
  let rec pi_start_state state strm =
    next_char strm not_eof (fun u ->
      if XName.is_name_start_char u then (
        add_chars state (E.encode_unicode u);
        pi_target_state state strm
      ) else
        error strm (Exn_CharToken u)
    )
  and pi_target_state state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        let target = extract_buffer state in
          before_pi_data_state target state strm
      else if u = u_qmark then (
        let target = extract_buffer state in
          consume_sequence strm [u_gt] >>= fun () ->
        S.return (target, "")
      ) else if XName.is_name_char u then (
        add_chars state (E.encode_unicode u);
        pi_target_state state strm
      ) else
          error strm (Exn_CharToken u)
    )
  and before_pi_data_state target state strm =
    next_char strm not_eof (fun u ->
      if is_space u then
        before_pi_data_state target state strm
      else if u = u_qmark then
        pi_data_qmark_state target state strm
      else (
        add_chars state (E.encode_unicode u);
        pi_data_state target state strm
      )
    )
  and pi_data_state target state strm =
    next_char strm not_eof (fun u ->
      if u = u_qmark then
        pi_data_qmark_state target state strm
      else (
        add_chars state (E.encode_unicode u);
        pi_data_state target state strm
      )
    )
  and pi_data_qmark_state target state strm =
    next_char strm not_eof (fun u ->
      if u = u_gt then
        let data = extract_buffer state in
          S.return (target, data)
      else (
        add_chars state (E.encode_unicode u_qmark);
        add_chars state (E.encode_unicode u);
        pi_data_state target state strm
      )
    )

  let doctype_state state strm =
    let rec start state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          start state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_name_state state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_name_state state strm =
      next_char strm not_eof (fun u ->
        if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_name_state state strm
        ) else if is_space u then
            let name = extract_buffer state in
            let doctype = {
              dtd_name = name;
              dtd_external_id = None;
              dtd = []
            } in
              doctype_external_id_state doctype state strm
          else if u = u_gt then
            let name = extract_buffer state in
            let doctype = {
              dtd_name = name;
              dtd_external_id = None;
              dtd = []
            } in
              X.emit_doctype doctype
          else if u = u_lbracket then
            let name = extract_buffer state in
            let doctype = {
              dtd_name = name;
              dtd_external_id = None;
              dtd = []
            } in
              doctype_intsubsect_state doctype state strm
          else
            error strm (Exn_CharToken u)
      )
    and doctype_external_id_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_external_id_state doctype state strm
        else if u = u_gt then
          X.emit_doctype doctype
        else if u = u_S then (
          consume_sequence strm [u_Y; u_S; u_T; u_E; u_M] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_systemliteral_state state strm >>= fun systemid ->
          doctype_before_intsubsect_state
            {doctype with dtd_external_id = Some (SystemID systemid)} state strm
        ) else if u = u_P then (
          consume_sequence strm [u_U; u_B; u_L; u_I; u_C] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_publicliteral_state state strm >>=
            fun (public, system) ->
          doctype_before_intsubsect_state {doctype with
            dtd_external_id = Some (PublicID (public, system))} state strm
        ) else if u = u_lbracket then
          doctype_intsubsect_state doctype state strm
          else
            error strm (Exn_CharToken u)
      )
    and doctype_before_systemliteral_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_before_systemliteral_state state strm
        else if u = u_quot || u = u_apos then
          doctype_systemliteral_state u state strm 
        else
          error strm (Exn_CharToken u)
      )
    and doctype_systemliteral_state lim state strm =
      next_char strm not_eof (fun u ->
        if u = lim then
          let value = extract_buffer state in
            S.return value
        else (
          add_chars state (E.encode_unicode u);
          doctype_systemliteral_state lim state strm
        )
      )
    and doctype_before_publicliteral_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_before_publicliteral_state state strm
        else if u = u_quot || u = u_apos then
          doctype_publicliteral_state u state strm
        else
          error strm (Exn_CharToken u)
      )
    and doctype_publicliteral_state lim state strm =
      next_char strm not_eof (fun u ->
        if u = lim then (
          let value = extract_buffer state in
            consume_space strm >>= fun () ->
          doctype_before_systemliteral_state state strm >>= fun systemid ->
            S.return (value, systemid)
        ) else if u = 0x0020 || u = 0x000D || u = 0x000A ||
                 char_range u 0x0061 0x007A || char_range u 0x0041 0x005A ||
                 char_range u 0x0030 0x0039 ||
                 one_of u [u_dash; u_apos; u_lparen; u_rparen; u_plus;
                           u_comma; u_dot; u_slash; u_colon; u_eq; u_qmark;
                           u_semicolon; u_excl; u_star; u_sharp; u_at;
                           u_dollar; u_underline; u_percent] then (
                   add_chars state (E.encode_unicode u);
                   doctype_publicliteral_state lim state strm
                 )
          else
            error strm (Exn_CharToken u)
      )
    and doctype_before_intsubsect_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_before_intsubsect_state doctype state strm
        else if u = u_lbracket then
          doctype_intsubsect_state doctype state strm
        else
          error strm (Exn_CharToken u)
      )
    and doctype_intsubsect_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_intsubsect_state doctype state strm
        else if u = u_rbracket then
          doctype_intsubsect_end_state doctype state strm
        else if u = u_percent then
          doctype_pereference_start_state doctype state strm
        else if u = u_lt then
          doctype_markupdecl_state doctype state strm
        else
          error strm (Exn_CharToken u)
      )
    and doctype_intsubsect_end_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_intsubsect_end_state doctype state strm
        else if u = u_gt then
          X.emit_doctype doctype
        else
          error strm (Exn_CharToken u)
      )
    and doctype_pereference_start_state doctype state strm =
      next_char strm not_eof (fun u ->
        if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_pereference_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_pereference_state doctype state strm =
      next_char strm not_eof (fun u ->
        if u = u_semicolon then
          let name = extract_buffer state in
            doctype_intsubsect_state {doctype with
              dtd = DTD_PEReference name :: doctype.dtd} state strm
        else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_pereference_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_markupdecl_state doctype state strm =
      next_char strm not_eof (fun u ->
        if u = u_excl then
          doctype_markupdecl_excl_state doctype state strm
        else if u = u_qmark then (
          pi_start_state state strm >>= fun (target, data) ->
          doctype_intsubsect_state {doctype with
            dtd = DTD_PI (target, data) :: doctype.dtd} state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_markupdecl_excl_state doctype state strm =
      next_char strm not_eof (fun u ->
        if u = u_dash then (
          consume_sequence strm [u_dash] >>= fun () ->
          comment_state state strm >>= fun () ->
          doctype_intsubsect_state doctype state strm
        ) else if u = u_A then (
          consume_sequence strm [u_T; u_T; u_L; u_I; u_S; u_T] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_attlist_state doctype state strm
        ) else if u = u_E then (
          next_char strm not_eof (fun u ->
            if u = u_N then (
              consume_sequence strm [u_T; u_I; u_T; u_Y] >>= fun () ->
              consume_space strm >>= fun () ->
              doctype_entity_state doctype state strm
            ) else if u = u_L then (
              consume_sequence strm [u_E; u_M; u_E; u_N; u_T] >>= fun () ->
              consume_space strm >>= fun () ->
              doctype_element_state doctype state strm
            ) else
                error strm (Exn_CharToken u)
          )
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_attlist_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_attlist_state doctype state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_attlist_name_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_attlist_name_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then (
          let name = extract_buffer state in
            doctype_attlist_attdef_state [] state strm >>= fun defs ->
            doctype_intsubsect_state {doctype with
              dtd = DTD_ATTLIST (name, defs) :: doctype.dtd} state strm
        ) else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_attlist_name_state doctype state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and doctype_attlist_attdef_state defs state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_attlist_attdef_state defs state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_attlist_attdef_name_state state strm >>= fun attdef ->
          doctype_after_attdef_state (attdef :: defs) state strm
        ) else if u = u_gt then
          S.return defs
          else
            error strm (Exn_CharToken u)
      )
    and doctype_attlist_attdef_name_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then (
          let attrname = extract_buffer state in
            doctype_attlist_atttype_state state strm >>= fun atttype ->
          doctype_attlist_defaultdecl state strm >>= fun defaultdecl ->
            S.return (attrname, atttype, defaultdecl)
        ) else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_attlist_attdef_name_state state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and doctype_attlist_atttype_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_attlist_atttype_state state strm
        else if u = u_C then (
          consume_sequence strm [u_D; u_A; u_T; u_A] >>= fun () ->
          consume_space strm >>= fun () ->
          S.return `CDATA
        ) else if u = u_I then (
          consume_sequence strm [u_D] >>= fun () ->
          consume_space strm >>= fun () ->
          S.return `ID
        ) else if u = u_N then (
          consume_sequence strm [u_M; u_T; u_O; u_K; u_E; u_N] >>= fun () ->
          consume_space strm >>= fun () ->
          S.return `NMTOKEN
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_attlist_defaultdecl state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_attlist_defaultdecl state strm
        else if u = u_sharp then (
          next_char strm not_eof (fun u ->
            if u = u_R then (
              consume_sequence strm [u_E; u_Q; u_U; u_I; u_R; u_E; u_D] >>=
                fun () -> S.return `Required
            ) else if u = u_I then (
              consume_sequence strm [u_M; u_P; u_L; u_I; u_E; u_D] >>= fun () ->
              S.return `Implied
            ) else
                error strm (Exn_CharToken u)
          )
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_after_attdef_state defs state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
            doctype_attlist_attdef_state defs state strm
        else if u = u_gt then
          S.return defs
        else
          error strm (Exn_CharToken u)
      )
    and doctype_entity_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_entity_state doctype state strm
        else if u = u_percent then (
          consume_space strm >>= fun () ->
          doctype_entity_pedecl_state doctype state strm
        ) else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_entity_gedecl_state doctype state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and doctype_entity_pedecl_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_entity_pedecl_state doctype state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_entity_pedecl_name_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_entity_pedecl_name_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then (
          let name = extract_buffer state in
            before_doctype_pedef_state doctype name state strm
        ) else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_entity_pedecl_name_state doctype state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and before_doctype_pedef_state doctype name state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          before_doctype_pedef_state doctype name state strm
        else if u = u_quot || u = u_apos then (
          doctype_entityvalue_state u state strm >>= fun value ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (PEDecl (name, `EntityValue value)) :: doctype.dtd}
            state strm
        ) else if u = u_S then (
          consume_sequence strm [u_Y; u_S; u_T; u_E; u_M] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_systemliteral_state state strm >>= fun system ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (PEDecl (name,
                                      (`ExternalID (SystemID system))))
            :: doctype.dtd}
            state strm
        ) else if u = u_P then (
          consume_sequence strm [u_U; u_B; u_L; u_I; u_C] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_publicliteral_state state strm >>=
            fun (public, system) ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (PEDecl (name, 
                                      (`ExternalID (PublicID (public, system)))))
            :: doctype.dtd} state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and doctype_entityvalue_state lim state strm =
      next_char strm not_eof (fun u ->
        if u = lim then
          let value = extract_buffer state in
            S.return value
        else (
          (* TODO: Pereference, Reference *)
          add_chars state (E.encode_unicode u);
          doctype_entityvalue_state lim state strm
        )
      )
    and doctype_entity_gedecl_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          let name = extract_buffer state in
            doctype_gedecl_entitydef_state doctype name state strm
        else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_entity_gedecl_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_gedecl_entitydef_state doctype name state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_gedecl_entitydef_state doctype name state strm
        else if u = u_quot || u = u_apos then (
          doctype_entityvalue_state u state strm >>= fun value ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (GEDecl (name, `EntityValue value)) :: doctype.dtd}
            state strm
        ) else if u = u_S then (
          consume_sequence strm [u_Y; u_S; u_T; u_E; u_M] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_systemliteral_state state strm >>= fun system ->
          doctype_gedecl_notion_state state strm >>= fun notion ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (GEDecl (name,
                                      `ExternalID (SystemID system, notion)))
              :: doctype.dtd} state strm
        ) else if u = u_P then (
          consume_sequence strm [u_U; u_B; u_L; u_I; u_C] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_before_publicliteral_state state strm >>=
            fun (public, system) ->
          doctype_gedecl_notion_state state strm >>= fun notion ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Entity (GEDecl (name,
                                      `ExternalID ((PublicID (public, system)),
                                                   notion))) :: doctype.dtd}
            state strm
        ) else
            error strm (Exn_CharToken u)
      )
    and doctype_gedecl_notion_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_gedecl_notion_space_state state strm
        else if u = u_gt then
          S.return None
        else
          error strm (Exn_CharToken u)
      )
    and doctype_gedecl_notion_space_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_gedecl_notion_space_state state strm
        else if u = u_gt then
          S.return None
        else if u = u_N then (
          consume_sequence strm [u_D; u_A; u_T; u_A] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_gedecl_notion_before_name_state state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_gedecl_notion_before_name_state state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_gedecl_notion_before_name_state state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_gedecl_notion_name state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_gedecl_notion_name state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          let name = extract_buffer state in
            S.return (Some name)
        else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_gedecl_notion_name state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_element_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_element_state doctype state strm
        else if XName.is_name_start_char u then (
          add_chars state (E.encode_unicode u);
          doctype_element_name_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_element_name_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          let name = extract_buffer state in
            doctype_element_contentspec_state doctype name state strm
        else if XName.is_name_char u then (
          add_chars state (E.encode_unicode u);
          doctype_element_name_state doctype state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and doctype_element_contentspec_state doctype name state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_element_contentspec_state doctype name state strm
        else if u = u_E then (
          consume_sequence strm [u_M; u_P; u_T; u_Y] >>= fun () ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Element (name, `Empty) :: doctype.dtd} state strm
        ) else if u = u_A then (
          consume_sequence strm [u_N; u_Y] >>= fun () ->
          doctype_markupdecl_end_state {doctype with
            dtd = DTD_Element (name, `Any) :: doctype.dtd} state strm
        ) else
            (* TODO *)
            error strm (Exn_CharToken u)
      )
      
    and doctype_markupdecl_end_state doctype state strm =
      next_char strm not_eof (fun u ->
        if is_space u then
          doctype_markupdecl_end_state doctype state strm
        else if u = u_gt then
          doctype_intsubsect_state doctype state strm
        else
          error strm (Exn_CharToken u)
      )
          
    in
      start state strm

  let prolog_state state strm =
    let rec start state strm =
      next_char strm not_eof (fun u ->
        if u = u_lt then
          prolog_less_than_sign_state state strm
        else if is_space u then (
          state.next_state <- PrologMiscState;
          start state strm
        ) else
          error strm (Exn_CharToken u)
      )
    and prolog_less_than_sign_state state strm =
      next_char strm not_eof (fun u ->
        if u = u_qmark then (
          pi_start_state state strm >>= fun (target, data) ->
          if target = "xml" then
            if state.next_state = PrologXmlDeclState then (
              let _version, encoding, _standalone = parse_xmldecl data in (
                match encoding with
                  | Some encname -> set_decoder encname strm;
                  | None -> ()
              );
                state.next_state <- PrologMiscState;
                start state strm
            ) else
              error strm (Exn_msg "Illegal PI target")
          else
            X.emit_pi target data
        ) else if u = u_excl then
            prolog_markup_state state strm
          else if XName.is_name_start_char u then (
            state.next_state <- LessThanSignState;
            add_chars state (E.encode_unicode u);
            start_tag_name_state state strm
          ) else
            error strm (Exn_CharToken u)
      )

    and prolog_markup_state state strm =
      next_char strm not_eof (fun u ->
        if u = u_dash then (
          consume_sequence strm [u_dash] >>= fun () ->
          comment_state state strm >>= fun () -> start state strm
        ) else if u = u_D then (
          consume_sequence strm [u_O; u_C; u_T; u_Y; u_P; u_E] >>= fun () ->
          consume_space strm >>= fun () ->
          doctype_state state strm
        ) else
            error strm (Exn_CharToken u)
      )
        
    in
      start state strm

  let rec cdata_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_rbracket then
        cdata_rbracket_state state strm
      else (
        add_chars state (E.encode_unicode u);
        cdata_state state strm
      )
    )
  and cdata_rbracket_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_rbracket then
        cdata_rbracket_rbracket_state state strm
      else (
        add_chars state (E.encode_unicode u_rbracket);
        add_chars state (E.encode_unicode u);
        cdata_state state strm
      )
    )
  and cdata_rbracket_rbracket_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_gt then
        let txt = extract_buffer state in
          state.next_state <- TextState;
          X.emit_text txt
      else (
        add_chars state (E.encode_unicode u_rbracket);
        add_chars state (E.encode_unicode u_rbracket);
        add_chars state (E.encode_unicode u);
        cdata_state state strm
      )
    )

  let markup_start_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_lbracket then (
        consume_sequence strm [u_C; u_D; u_A; u_T; u_A; u_lbracket] >>= fun () ->
        cdata_state state strm
      ) else if u = u_dash then (
        consume_sequence strm [u_dash] >>= fun () ->
        comment_state state strm >>= fun () ->
        text_state state strm
      ) else
          error strm (Exn_CharToken u)
    )
          
      
  let less_than_sign_state state strm =
    next_char strm not_eof (fun u ->
      if u = u_slash then
        end_tag_start_state state strm
      else if u = u_excl then
        markup_start_state state strm
      else if u = u_qmark then (
        pi_start_state state strm >>= fun (target, data) ->
        X.emit_pi target data
      ) else if XName.is_name_start_char u then (
        add_chars state (E.encode_unicode u);
        start_tag_name_state state strm
      )
        else
          error strm (Exn_CharToken u)
    )
      
  let rec after_element_state state strm =
      next_char strm X.emit_eof (fun u ->
        if is_space u then
          after_element_state state strm
        else if u = u_lt then (
          next_char strm not_eof (fun u ->
            if u = u_qmark then (
              pi_start_state state strm >>= fun (target, data) ->
              X.emit_pi target data
            ) else if u = u_excl then (
              consume_sequence strm [u_dash; u_dash] >>= fun () ->
              comment_state state strm >>= fun () ->
                after_element_state state strm
            ) else
                error strm (Exn_CharToken u)
          )
        ) else
          error strm (Exn_CharToken u)
      )

  let tokenizer state strm =
    match state.next_state with
      | TextState ->
        text_state state strm
      | LessThanSignState ->
        less_than_sign_state state strm
      | AfterElement ->
        after_element_state state strm
      | PrologXmlDeclState ->
        prolog_state state strm
      | PrologMiscState ->
        prolog_state state strm
        

  let make_lexer strm =
    let state = {
      tmp_buffer = Buffer.create 30;
      stack = Stack.create ();
      next_state = PrologXmlDeclState
    } in
      fun () -> tokenizer state strm
end
