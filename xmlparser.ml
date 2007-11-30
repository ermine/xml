(*
 * (c) 2007 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Fstream
open Encoding

exception LexerError of string
exception UnknownEntity of string

type production =
   | StartElement of string * (string * string) list
   | EndElement of string
   | EmptyElement of string * (string * string) list
   | Pi of string * string
   | Comment of string
   | Whitespace of string
   | Cdata of string
   | Text of string
   | Doctype of string * Xml.external_id option * string
   | EOD
(*   | EOB *)

type cb = production -> ('a -> unit) -> unit as 'a

type parser_t = {
   mutable encoding : string;
   mutable fparser : parser_t -> cb -> unit;
   mutable fencoder : int -> (int, char list) Fstream.t;
   mutable strm : char Stream.t;
   mutable nextf :  cb;
   entity_handler : string -> int;
   encoding_handler : string -> (char -> (char, int) Fstream.t);
}
and lstream = | Lexer of (parser_t -> int -> lstream) 
		 | Switch of 
		      (char -> (char, int) Fstream.t) * 
			 (parser_t -> int -> lstream)
		 | Token of production * (parser_t -> int -> lstream)

let accept_if str f =
   let len = String.length str in
   let rec aux_accept i state ch =
      if i < len then
	 if ch = Uchar.of_char str.[i] then
	    Lexer (aux_accept (i+1))
	 else 
	    raise (LexerError (Printf.sprintf "expected '%s'" str))
      else
	 f state ch
   in
      Lexer (aux_accept 0)

let rec skip_blank f state ucs4 =
   if Xmlchar.is_blank ucs4 then
      Lexer (skip_blank f)
   else
      f state ucs4

let after_blank nextf state ucs4 =
   if Xmlchar.is_blank ucs4 then
      Lexer (skip_blank nextf)
   else
      raise (LexerError "expected space")

let fencoder state buf ucs4 =
   match state.fencoder ucs4 with
      | F f -> state.fencoder <- f
      | R (r, f) ->
	   List.iter (fun c -> Buffer.add_char buf c) r;
	   state.fencoder <- f

(*
let parse_ncname nextf state ucs4 =
   let buf = Buffer.create 30 in
   let rec get_name state ucs4 =
      if Xmlchar.is_ncnamechar ucs4 then (
         fencoder state buf ucs4;
         Lexer get_name
      ) else (
         let name = Buffer.contents buf in
            Buffer.clear buf;
            nextf name state ucs4
      )
   in
      if Xmlchar.is_first_ncnamechar ucs4 then (
	 fencoder state buf ucs4;
	 Lexer get_name
      )
      else
	 raise (LexerError "invalid name")

let parse_qname nextf state ucs4 =
   parse_ncname (fun ncname1 state ucs4 ->
		    if ucs4 = Uchar.u_colon then
		       parse_ncname (fun ncname2 state ucs4 ->
					nextf (ncname1, ncname2) state ucs4)
			  state ucs4
		    else
		       nextf ("", ncname1) state ucs4
		) state ucs4
*)

let parse_name nextf state ucs4 =
   let buf = Buffer.create 30 in
   let rec get_name state ucs4 =
      if Xmlchar.is_namechar ucs4 then (
         fencoder state buf ucs4;
         Lexer get_name
      ) else (
         let name = Buffer.contents buf in
            Buffer.clear buf;
            nextf name state ucs4
      )
   in
      if Xmlchar.is_first_namechar ucs4 then (
	 fencoder state buf ucs4;
	 Lexer get_name
      )
      else (
	 fencoder state buf ucs4;
	 raise (LexerError ("invalid name '" ^ Buffer.contents buf ^ "'"))
      )
      
let parse_decimal_charref nextf state ucs4 =
   let rec get_decimal acc state ucs4 =
      if ucs4 >= Uchar.of_char '0' && ucs4 <= Uchar.of_char '9' then
	 Lexer (get_decimal (acc * 10 + (ucs4 - Uchar.of_char '0')))
      else if ucs4 = Uchar.u_semicolon then
	 nextf acc
      else
	 raise (LexerError "malformed character reference")
   in
      get_decimal 0 state ucs4

let parse_hexacimal_charref nextf =
   let rec get_decimal acc state ucs4 =
      if ucs4 >= Uchar.of_char '0' && ucs4 <= Uchar.of_char '9' then
	 Lexer (get_decimal (acc * 16 + (ucs4 - Uchar.of_char '0')))
      else if ucs4 >= Uchar.of_char 'A' && ucs4 <= Uchar.of_char 'F' then
	 Lexer (get_decimal (acc * 16 + (ucs4 - Uchar.of_char 'A' + 10)))
      else if ucs4 >= Uchar.of_char 'a' && ucs4 <= Uchar.of_char 'f' then
	 Lexer (get_decimal (acc * 16 + (ucs4 - Uchar.of_char 'a' + 10)))
      else if ucs4 = Uchar.u_semicolon then
	 nextf state acc
      else
	 raise (LexerError "malformed character reference")
   in
      Lexer (get_decimal 0)

let parse_reference buf nextf state ucs4 =
   if ucs4 = Uchar.of_char '#' then
      Lexer (fun state ucs4 ->
		if ucs4 = Uchar.of_char 'x' then
		   parse_hexacimal_charref (fun state ucs4 ->
					       fencoder state buf ucs4;
					       Lexer nextf
					   )
		else
		   parse_decimal_charref 
		      (fun ucs4 -> 
			  fencoder state buf ucs4;
			  Lexer nextf
		      ) state ucs4
	    )
   else
      parse_name 
	 (fun name state ucs4 ->
	     if ucs4 = Uchar.u_semicolon then (
		(match name with
		    | "lt" -> Buffer.add_char buf '<'
		    | "gt" -> Buffer.add_char buf '>'
		    | "apos" -> Buffer.add_char buf '\''
		    | "quot" -> Buffer.add_char buf '"'
		    | "amp" -> Buffer.add_char buf '&'
		    | other ->
			 let ucs4 = state.entity_handler other in
			    fencoder state buf ucs4
		);
		Lexer nextf
	     ) else
		raise (LexerError 
			  ("invalid reference: expecting ':' after '&" ^ name
			   ^ "'"))
	 ) state ucs4
	 
let parse_text nextf state ucs4 =
   let buf = Buffer.create 30 in
   let rec get_text state ucs4 =
      if ucs4 = Uchar.u_lt then (
	 let text = Buffer.contents buf in
	    Buffer.clear buf;
	    nextf text ucs4
      ) else if ucs4 = Uchar.u_closebr then
	 Lexer (fun state ucs42 ->
		   if ucs42 = Uchar.u_lt then
		      let text = Buffer.contents buf in
			 Buffer.clear buf;
			 nextf text ucs42
		   else if ucs42 = Uchar.u_closebr then
		      Lexer (fun _state ucs43 ->
				if ucs43 = Uchar.u_gt then
				   raise (LexerError 
					     "']]>' is not allowed in text")
				else (
				   fencoder state buf ucs4;
				   fencoder state buf ucs42;
				   fencoder state buf ucs43;
				   Lexer get_text
				)
			    )
		   else (
		      fencoder state buf ucs4;
		      fencoder state buf ucs42;
		      Lexer get_text
		   )
	       )
      else if ucs4 = Uchar.u_amp then
	 Lexer (parse_reference buf get_text)
      else (
	 fencoder state buf ucs4;
	 Lexer get_text
      )
   in
      get_text state ucs4

let parse_whitespace nextf state ucs4 =
   let buf = Buffer.create 10 in
   let rec get_spaces state (ucs4:int) =
      if Xmlchar.is_blank ucs4 then (
	 fencoder state buf ucs4;
	 Lexer get_spaces
      )
      else
	 let text = Buffer.contents buf in
	    Buffer.clear buf;
	    nextf text ucs4
   in
      fencoder state buf ucs4;
      Lexer get_spaces

let parse_comment nextf state ucs4 =
   let buf = Buffer.create 30 in
   if ucs4 = Uchar.u_dash then
      let rec get_comment state ucs41 =
	 if ucs41 = Uchar.u_dash then
	    Lexer (fun _state ucs42 ->
		      if ucs42 = Uchar.u_dash then
			 Lexer (fun _state ucs43 ->
				   if ucs43 = Uchar.u_gt then
				      let comment = Buffer.contents buf in
					 Buffer.clear buf;
					 nextf comment
				   else
				      raise (LexerError 
					"-- is not allowed inside comment")
			       )
		      else (
			 fencoder state buf ucs41;
			 fencoder state buf ucs42;
			 Lexer get_comment
		      )
		  )
	 else (
	    fencoder state buf ucs41;
	    Lexer get_comment
	 )
      in
	 Lexer get_comment
   else
      raise (LexerError "Malformed cooment")

let parse_string nextf state ucs4 =
   if ucs4 = Uchar.of_char '"' || ucs4 = Uchar.of_char '\'' then
      let buf = Buffer.create 30 in
      let rec get_text qt state ucs4 =
	 if ucs4 = qt then
	    let str = Buffer.contents buf in
	       Buffer.clear buf;
	       nextf str
	 else (
	    fencoder state buf ucs4;
	    Lexer (get_text qt)
	 )
      in
	 Lexer (get_text ucs4)
   else
      raise (LexerError "expected string")
	 
let parse_external_id nextf state ucs4 =
   if ucs4 = Uchar.of_char 'S' then
      accept_if "YSTEM" 
	 (after_blank (parse_string 
			  (fun str -> Lexer (nextf (Some (`System str))))))
   else if ucs4 = Uchar.of_char 'P' then
      accept_if "UBLIC"
	 (after_blank 
	     (parse_string 
		 (fun str -> 
		     Lexer 
			(after_blank 
			    (parse_string 
				(fun str2 ->
				    Lexer (nextf (Some (`Public (str, str2))))
				))))))
   else
      nextf None state ucs4

let parse_doctype nextf =
   accept_if "OCTYPE "
      (skip_blank 
	  (fun state ucs4 ->
	      parse_name (fun name state ucs4 ->
			     if ucs4 = Uchar.of_char '>' then
				nextf name None ""
			     else if Xmlchar.is_blank ucs4 then
				Lexer (skip_blank (parse_external_id
					  (fun ext ->
					  (fun state ucs4 ->
					      let buf = Buffer.create 30 in
					      let rec get_text state ucs4 =
						 if ucs4 = Uchar.of_char '>' then
						    let text = 
						       Buffer.contents buf in
						       Buffer.clear buf;
						       nextf name ext text
						 else (
						    fencoder state buf ucs4;
						    Lexer get_text
						 )
					      in
						 get_text state ucs4
					  )))
				      )
			     else
				raise (LexerError "bad doctype syntax")
			 ) state ucs4
	  )
      )
	 
(* Attribute Value *)
(*  Implements also (partially) 3.3.3 Attribute-Value Normalization *)
let parse_attrvalue nextf =
   let buf = Buffer.create 30 in
   let rec get_value qt state ucs4 =
      if ucs4 = qt then
	 let value = Buffer.contents buf in
	    Buffer.clear buf;
	    Lexer (nextf value)
      else if ucs4 = Uchar.u_amp then
	 Lexer (parse_reference buf (get_value qt))
      else if ucs4 = Uchar.u_lf || ucs4 = Uchar.u_tab then (
	 fencoder state buf ucs4;
	 Lexer (get_value qt)
      )
      else (
	 fencoder state buf ucs4;
	 Lexer (get_value qt)
      )
   in
      Lexer (fun state ucs4 ->
		if ucs4 = Uchar.u_apos || ucs4 = Uchar.u_quot then
		   Lexer (get_value ucs4)
		else
		   raise (LexerError "malformed attribute value")
	    )

(*
 * [41] Attribute ::= Name Eq AttValue
 *)
let rec parse_attributes tag attrs nextf state ucs4 =
   if ucs4 = Uchar.u_gt then
      nextf tag attrs true
   else if ucs4 = Uchar.u_slash then
      Lexer (fun state ucs4 ->
		if ucs4 = Uchar.u_gt then
		   nextf tag attrs false
		else
		   raise (LexerError "invalid end of start tag")
	    )
   else 
      let smth state ucs4 =
	 if ucs4 = Uchar.u_gt then
	    nextf tag attrs true
	 else if ucs4 = Uchar.u_slash then
	    Lexer (fun state ucs4 ->
		      if ucs4 = Uchar.u_gt then
			 nextf tag attrs false
		      else
			 raise (LexerError "bad end of empty tag")
		  )
	 else 
	    parse_name
	       (fun name state ucs4 ->
		   if ucs4 = Uchar.u_eq then
		      parse_attrvalue
			 (fun value state ucs4 ->
			     parse_attributes tag 
				((name, value) :: attrs) nextf state ucs4
			 )
		   else
		      raise (LexerError "expected =")
	       ) state ucs4
      in
	 after_blank smth state ucs4

let parse_start_element nextf state ucs4 =
   parse_name (fun name -> parse_attributes name [] nextf) state ucs4

let parse_end_element nextf state ucs4 =
   parse_name
      (fun name ->
	  skip_blank (fun state ucs4 ->
			 if ucs4 = Uchar.u_gt then
			    nextf name
			 else
			    raise (LexerError "bad closing tag")
		     )) state ucs4
	 
let parse_cdata nextf =
   let buf = Buffer.create 30 in
      accept_if "CDATA["
	 (fun state ucs4 ->
	     let rec get_cdata state ch1 =
		if ch1 = Uchar.of_char ']' then
		   Lexer (fun state ch2 ->
			     if ch2 = Uchar.of_char ']' then
				Lexer (fun state ch3 ->
					  if ch3 = Uchar.of_char '>' then
					     let cdata = Buffer.contents buf in
						Buffer.clear buf;
						nextf cdata
					  else
					     raise (LexerError "expected ']]>'"))
			     else (
				fencoder state buf ch1;
				fencoder state buf ch2;
				Lexer get_cdata
			     )
			 )
		else (
		   fencoder state buf ch1;
		   Lexer get_cdata
		)
	     in
		get_cdata state ucs4
	 )
	 
let parse_pi nextf state ucs4 =
   parse_name 
      (fun target state ucs4 ->
	  let buf = Buffer.create 30 in
	  let rec get_pi_content state ucs41 =
	     if ucs41 = Uchar.u_quest then
		Lexer (fun state ucs42 ->
			  if ucs42 = Uchar.u_gt then
			     let data = Buffer.contents buf in
				Buffer.clear buf;
				nextf target data
			  else (
			     fencoder state buf ucs41;
			     fencoder state buf ucs42;
			     Lexer get_pi_content
			  )
		      )
	     else (
		fencoder state buf ucs41;
		Lexer get_pi_content
	     )
	  in
	     after_blank get_pi_content state ucs4
      ) state ucs4
      
(*
 *  [23] XMLDecl     ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>' 
 * [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
 * [25] Eq          ::= S? '=' S?
 * [26] VersionNum  ::= '1.0'
 *)
let parse_xmldecl state data =
   let ua = uarray_of_utf8_string data in
   let s i = 
      if i < Array.length ua then ua.(i) 
      else raise (LexerError ("bad xml declaration: " ^ data))
   in
   let buf = Buffer.create 30 in
   let ascii_letter ucs4 =
      ucs4 >= Uchar.of_char 'a' && ucs4 <= Uchar.of_char 'z' in
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
      if i < Array.length ua && Xmlchar.is_blank ua.(i) then
	 skip_blank (i+1)
      else i
   in
   let rec get_attrs acc i =
      if i < Array.length ua then
	 let name, i = get_name i in
	 let i = skip_blank i in
	    if s i = Uchar.u_eq then
	       let i = skip_blank (i+1) in
		  if s i = Uchar.u_apos || s i = Uchar.u_quot then
		     let value, i = get_value (s i) (i+1) in
			if (i+1) < Array.length ua &&
			   Xmlchar.is_blank ua.(i+1) then
			      let i = skip_blank (i+1) in
				 get_attrs ((name, value) :: acc) i
			else
			   ((name, value) :: acc)
		  else
		     raise (LexerError ("bad xml declaration: " ^ data))
	    else
	       raise (LexerError ("bad xml declaration: " ^ data))
      else
	 acc
   in
   let attrs = get_attrs [] 0 in
      List.rev attrs

let rec lexer state ucs4 =
   let opened_lt state ucs4 =
      if ucs4 = Uchar.u_slash then
	 Lexer (parse_end_element (fun name -> Token (EndElement name, lexer)))
      else if ucs4 = Uchar.u_quest then
	 Lexer (parse_pi (fun target data -> Token (Pi (target, data), lexer)))
      else if ucs4 = Uchar.u_excl then
	 Lexer (fun state ucs4 ->
		   if ucs4 = Uchar.u_openbr then
		      parse_cdata (fun cdata -> Token (Cdata cdata, lexer))
		   else if ucs4 = Uchar.u_dash then
		      Lexer (parse_comment 
				(fun comment -> Token (Comment comment, lexer)))
		   else if ucs4 = Uchar.of_char 'D' then
		      parse_doctype
			 (fun name ext_id str -> 
			     Token (Doctype (name, ext_id, str), lexer))
		   else
		      raise (LexerError "unknown token <!.")
	       )
      else 
	 parse_start_element 
	    (fun name attrs flag ->
		if not flag then
		   Token (EmptyElement (name, (List.rev attrs)), lexer)
		else
		   Token (StartElement (name, (List.rev attrs)), lexer)
	    )
	    state ucs4
   in
      if ucs4 = Uchar.u_lt then
	 Lexer opened_lt
      else
	 if Xmlchar.is_blank ucs4 then
	    parse_whitespace 
	       (fun text ucs4 -> 
		   if ucs4 = Uchar.u_lt then
		      Token (Whitespace text, opened_lt)
		   else (
		      parse_text (fun text ucs4 ->
				     if ucs4 = Uchar.u_lt then
					Token (Text text, opened_lt)
				     else
					raise (LexerError "expected '<'"))
			 state ucs4
		   )
	       )
	       state ucs4
	 else
	    parse_text 
	       (fun text ucs4 -> 
		   if ucs4 = Uchar.u_lt then
		      Token (Text text, opened_lt)
		   else
		      raise (LexerError "expected '<'"))
	       state ucs4
	    
let process_xmldecl attrs state =
   let version = 
      try List.assoc "version" attrs with Not_found -> "" in
      if version <> "1.0" then
	 raise (LexerError ("unknown version of xml: '" ^ version ^ "'"));
      let encoding = try List.assoc "encoding" attrs with Not_found -> "" in
	 if encoding = "" then
	    Lexer lexer
	 else
	    let up = String.uppercase encoding in
	       if state.encoding = up then
		  Lexer lexer
	       else
		  let fdecoder =
		     match up with
			| "ASCII" | "US-ASCII" ->
			     Encoding.decode_ascii
			| "LATIN1" | "ISO-8859-1" ->
			     Encoding.decode_latin1
			| "UTF-8" ->
			     Encoding.decode_utf8
			| "UTF-16" | "UTF-16BE" ->
			     Encoding.decode_utf16 BE
			| "UTF-16LE" ->
			     Encoding.decode_utf16 BE
			| "UCS-4" | "UCS-4BE" ->
			     Encoding.decode_ucs4
			| "UCS-4LE" ->
			     Encoding.decode_ucs4le
			| other ->
			     state.encoding_handler encoding
		  in
		     Switch (fdecoder, lexer)

(* 2.8 Prolog and Document Type Declaration
 * 
 * [22] prolog      ::= XMLDecl? Misc* (doctypedecl Misc* )?
 * [27] Misc        ::= Comment | PI | S
 *)
let rec start_lexer state ucs4 =
   let opened_lt state ucs4 =
      if ucs4 = Uchar.u_slash then
	 Lexer (parse_end_element (fun name -> Token (EndElement name, lexer)))
      else if ucs4 = Uchar.u_quest then
	Lexer ( parse_pi (fun pi data -> 
			     if pi = "xml" then
				let attrs = parse_xmldecl state data in
				   process_xmldecl attrs state
			     else
				Token (Pi (pi, data), lexer)))
      else if ucs4 = Uchar.u_excl then
	 Lexer (fun state ucs4 ->
		   if ucs4 = Uchar.u_openbr then
		      parse_cdata (fun cdata -> Token (Cdata cdata, lexer))
		   else if ucs4 = Uchar.u_dash then
		      Lexer (parse_comment 
				(fun comment -> Token (Comment comment, lexer)))
		   else if ucs4 = Uchar.of_char 'D' then
		      parse_doctype
			 (fun name ext_id str -> 
			     Token (Doctype (name, ext_id, str), lexer))
		   else
		      raise (LexerError "unknown token <!.")
	       )
      else 
	 parse_start_element 
	    (fun name attrs flag ->
		if not flag then
		   Token (EmptyElement (name, (List.rev attrs)), lexer)
		else
		   Token (StartElement (name, (List.rev attrs)), lexer)
	    )
	    state ucs4
   in
      if ucs4 = Uchar.u_lt then
	 Lexer opened_lt
      else
	 if Xmlchar.is_blank ucs4 then
	    parse_whitespace 
	       (fun text ucs4 -> Token (Whitespace text, opened_lt))
	       state ucs4
	 else
	    raise (LexerError "Start tag expected, '<' not found")

(*
let debug_tag = function
   | StartElement (name, _attrs) ->
	Printf.printf "StartElement %s\n" name
   | EmptyElement (name, _attrs) ->
	Printf.printf "EmptyElement %s\n" name
   | EndElement name ->
	Printf.printf "EndElement %s\n" name;
   | Text text ->
	Printf.printf "Text %s\n" text
   | Cdata cdata ->
	Printf.printf "Cdata %s\n" cdata
   | Whitespace spaces ->
	Printf.printf "Whitespace [%s]" spaces
   | Pi (target, data) ->
	Printf.printf "Pi %s %s\n" target data;
   | Comment comment ->
	Printf.printf "Comment %s\n" comment;
   | Doctype _ ->
	Printf.printf "Doctype\n"
   | EOD ->
	Printf.printf "EOD"
*)

(* 2.11 End-of-Line Handling
 * \r\n -> \n
 * \r -> \n
 *)
let rec norm_nl ucs4 =
   if ucs4 = Uchar.u_cr then
      let rec aux_newline ch =
	 if ch = Uchar.u_lf then
	    R (Uchar.u_lf, norm_nl)
	 else if ch = Uchar.u_cr then
	    R (Uchar.u_lf, aux_newline)
	 else
	    R (Uchar.u_lf, norm_nl)
      in
	 F aux_newline
   else
      R (ucs4, norm_nl)

let rec fparser state fdecoder norm_nl flexer nextf=
   match Stream.peek state.strm with
      | Some ch ->
	   Stream.junk state.strm;
	   (match fdecoder ch with
	       | F fdecoder ->
		    fparser state fdecoder norm_nl flexer nextf
	       | R (ucs4, fdecoder) ->
		    match norm_nl ucs4 with
		       | F norm_nl ->
			    fparser state fdecoder norm_nl flexer nextf
		       | R (ucs4, norm_nl) ->
			    match flexer state ucs4 with
			       | Lexer flexer ->
				    fparser state fdecoder norm_nl flexer nextf
			       | Switch (fdecoder, flexer) ->
				    fparser state fdecoder norm_nl flexer nextf
			       | Token (tag, flexer) ->
				    nextf tag 
				       (fparser state fdecoder norm_nl flexer)
	   )
      | None ->
	   state.nextf <- nextf;
	   state.fparser <- fun state -> fparser state fdecoder norm_nl flexer
   
let prepare_fparser enc process_unknown_encoding =
   match enc with
      | "NONE" ->
	   let autodetect state =
	      let chs = Stream.npeek 4 state.strm in
		 if List.length chs < 4 then
		    raise TooFew;
		 let chs = Array.of_list chs in
		 let encoding, fdecoder = Encoding.autodetect_encoding 
		    (Uchar.of_char chs.(0)) (Uchar.of_char chs.(1)) 
		    (Uchar.of_char chs.(2)) (Uchar.of_char chs.(3))
		 in
		    state.encoding <- encoding;
		    fparser state fdecoder norm_nl start_lexer
	   in
	      autodetect
      | "UTF-8" -> 
	   (fun state -> fparser state Encoding.decode_utf8 norm_nl start_lexer)
      | "UTF-16" ->
	   (fun state ->
	       fparser state (Encoding.decode_utf16 Encoding.BE) 
		  norm_nl start_lexer)
      | "ASCII" ->
	   (fun state -> fparser state  Encoding.decode_ascii 
	       norm_nl start_lexer)
      | "LATIN1" ->
	   (fun state -> fparser state Encoding.decode_latin1 
	       norm_nl start_lexer)
      | "UCS-4" ->
	   (fun state -> fparser state  Encoding.decode_ucs4 
	       norm_nl start_lexer)
      | other ->
	   failwith ("Unsupported encoding " ^ other)

let create ?encoding 
      ?process_unknown_encoding
      ?process_entity
      ?process_production
      () =
   let entity_handler =
      match process_entity with
	 | None ->
	      (fun name -> raise (LexerError ("Unknown entity name " ^ name)))
	 | Some v ->
	      v
   in
   let encoding_handler =
      match process_unknown_encoding with
	 | None ->
	      (fun name -> raise (LexerError ("Unknown encoding " ^ name)))
	 | Some v ->
	      v
   in
   let enc = 
      match encoding with
	 | None -> "NONE"
	 | Some e ->
	      match e with
		 | Enc_UTF8 ->
		      "UTF-8"
		 | Enc_UTF16 ->
		      "UTF-16"
		 | Enc_ASCII ->
		      "ASCII"
		 | Enc_Latin1 ->
		      "LATIN1"
		 | Enc_UCS4 ->
		      "UCS-4"
   in			 
   let fparser = prepare_fparser enc encoding_handler in
   let nextf =
      match process_production with
	 | None -> 
	      let rec stub _tag f = f stub in stub
	 | Some v ->
	      v
   in
      {
	 encoding = enc;
	 fencoder = Encoding.encode_utf8;
	 fparser = fparser;
	 strm = Stream.of_string "";
	 nextf = nextf;
	 entity_handler = entity_handler;
	 encoding_handler = encoding_handler
      }

let set_callback state callback =
   state.nextf <- callback

let parse state str start len = 
   let strm = 
      Stream.from (fun c -> if c+start < len then Some str.[c+start] else None)
   in
      state.strm <- strm;
      state.fparser state state.nextf

let finish state =
   state.nextf EOD (fun _ -> failwith "XML Parser finished")

(*
   let strm = Stream.of_string "" in
      state.strm <- strm;
      state.fparser state true state.nextf
*)
	 
let reset state process_production =
   let fparser = prepare_fparser state.encoding state.encoding_handler in
      state.nextf <- process_production;
      state.fparser <- fparser

let get_rest_buffer state =
   let buf = Buffer.create 9216 in
      Stream.iter (Buffer.add_char buf) state.strm;
      Buffer.contents buf

let decode = Xml_decode.decode
let encode = Xml_encode.encode

let split_name name =
   if String.contains name ':' then
      let idx = String.index name ':' in
      let prefix = String.sub name 0 idx in
      let lname = 
	 if idx+1 > String.length name then
	    ""
	 else
	    String.sub name (idx+1) (String.length name - (idx+1))
      in
	 prefix, lname
   else
      "", name
