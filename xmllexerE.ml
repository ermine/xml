(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

open Xmllexer_generic

module IterMonad =
struct
  type input = {
    buf : string;
    mutable i : int;
    mutable len : int;
    mutable is_final : bool
  }

  let make_chunk size =
    { buf = String.create size; i = 0; len = 0; is_final = false}

  let is_empty s =
    s.i = s.len

  type 'a t =
    | Return of 'a
    | Continue of (input -> 'a t)

  let return x = Return x
  let fail = raise
  let rec bind v f =
    match v with
      | Return x -> f x
      | Continue cont -> Continue (fun s ->
        match cont s with
          | Return x -> f x
          | i -> bind i f
      )
    
  let (>>=) = bind

  exception IllegalCharacter
    
  let rec get s =
    if s.is_final then
      Return None
    else if s.i < s.len then
      let ch1 = s.buf.[s.i] in
        s.i <- s.i + 1;
        match ch1 with
          | '\000'..'\127' -> Return (Some (Char.code ch1))
          | '\192'..'\223' ->
            let rec cont s =
              if s.is_final then
                fail IllegalCharacter
              else if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i+1;
                  let n1 = Char.code ch1 in
                  let n2 = Char.code ch2 in
                    if (n2 lsr 6 != 0b10) then fail IllegalCharacter
                    else
                      let code = ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
                        Return (Some (code))
              else
                Continue cont
            in
              cont s
              
          | '\224'..'\239' ->
            let rec cont s =
              if s.is_final then
                fail IllegalCharacter
              else if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i + 1;
                  let rec cont2 s =
                    if s.is_final then
                      fail IllegalCharacter
                    else if s.i < s.len then
                      let ch3 = s.buf.[s.i] in
                        s.i <- s.i + 1;
                        let n1 = Char.code ch1
                        and n2 = Char.code ch2
                        and n3 = Char.code ch3 in
                          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
                            fail IllegalCharacter
                          else
                            let code = 
                              ((n1 land 0x0f) lsl 12) lor
                                ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
                            in
                              if (code >= 0xd800) && (code <= 0xdf00) then
                                fail IllegalCharacter
                              else Return (Some (code))
                    else
                      Continue cont2
                  in
                    cont2 s
              else
                Continue cont
            in
              cont s

          | '\240'..'\247' ->
            let rec cont s =
              if s.is_final then
                fail IllegalCharacter
              else if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i + 1;
                  let rec cont2 s =
                    if s.is_final then
                      fail IllegalCharacter
                    else if s.i < s.len then
                      let ch3 = s.buf.[s.i] in
                        s.i <- s.i + 1;
                        let rec cont3 s =
                          if s.is_final then
                            fail IllegalCharacter
                          else if s.i < s.len then
                            let ch4 = s.buf.[s.i] in
                              s.i <- s.i + 1;
                              let n1 = Char.code ch1
                              and n2 = Char.code ch2
                              and n3 = Char.code ch3
                              and n4 = Char.code ch4 in
                                if (n2 lsr 6 != 0b10) ||
                                  (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                                  fail IllegalCharacter
                                else
                                  Return (Some (((n1 land 0x07) lsl 18) lor
                                                   ((n2 land 0x3f) lsl 12) lor
                                                   ((n3 land 0x3f) lsl 6)
                                                lor (n4 land 0x3f)))
                          else
                            Continue cont3
                        in
                          cont3 s
                    else
                      Continue cont2
                  in
                    cont2 s
              else
                Continue cont
            in
              cont s

          | _ ->
            fail IllegalCharacter

    else
      Continue get

end

module Encoding =
struct
  exception IllegalCharacter

  let encode_unicode ucs4 =
    let bytes = 
      if ucs4 < 0x80 then
        [ucs4]
      else if ucs4 <= 0x7ff then
        [(0xc0 lor (ucs4 lsr 6)); (0x80 lor (ucs4 land 0x3f))]
      else if ucs4 <= 0xffff then (
        if (ucs4 >= 0xd800 & ucs4 < 0xe000) then 
          raise IllegalCharacter;
        [(0xe0 lor (ucs4 lsr 12));
         (0x80 lor ((ucs4 lsr 6) land 0x3f));
         (0x80 lor (ucs4 land 0x3f))
        ]
    )
      else if ucs4 <= 0x10ffff then
        [(0xf0 lor (ucs4 lsr 18));
         (0x80 lor ((ucs4 lsr 12) land 0x3f));
         (0x80 lor ((ucs4 lsr 6)  land 0x3f));
         (0x80 lor (ucs4 land 0x3f))]
      else 
        raise IllegalCharacter
    in
      List.map Char.chr bytes
end

module LocatedStream =
struct
  include IterMonad

  exception Located_exn of (int * int) * exn

  type source = IterMonad.input

  type stream = {
    mutable line : int;
    mutable col : int;
    mutable decoder : source -> int option t;
    source : source
  }

  let set_decoder encname strm = ()

  let make_stream source =
    { line = 0;
      col = 0;
      decoder = get;
      source = source
    }

  let error strm exn =
    fail (Located_exn ((strm.line, strm.col), exn))

  let next_char strm eof f =
    strm.decoder strm.source >>= function
      | Some u ->
        if u = 0x000A then (
          strm.line <- strm.line + 1;
          strm.col <- 0
        ) else
          strm.col <- strm.col + 1;
        f u
      | None -> eof ()
end

module XmlStanza (M : MONAD) =
struct
  type data =
    | StartTag of string * (string * string) list * bool
    | EndTag of string
    | Doctype of doctype
    | PI of string * string
    | Text of string

  type token = data option
  type 'a t = 'a M.t

  let emit_start_tag name attrs selfclosing =
    M.return (Some (StartTag (name, attrs, selfclosing)))

  let emit_end_tag name =
    M.return (Some (EndTag name))

  let emit_doctype doctype =
    M.return (Some (Doctype doctype))

  let emit_pi target data =
    M.return (Some (PI (target, data)))

  let emit_text text =
    M.return (Some (Text text))

  let emit_eof () =
    M.return None
end

module X = XmlStanza(IterMonad)

module M = Make
  (LocatedStream)
  (Encoding)
  (X)
  
open IterMonad
  


let parse_document inc =
  let source = make_chunk 8192 in
  let stream = M.S.make_stream source in
  let next_token = M.make_lexer stream in
  let namespaces = Hashtbl.create 1 in
  let () = Hashtbl.add namespaces "xml" Xml.ns_xml in
  let stack = Stack.create () in
  let stack_ns = Stack.create () in
  let add_element el =
    let (qname, attrs, subels) = Stack.pop stack in
      Stack.push (qname, attrs, (el :: subels)) stack
  in
  let rec process_token = function
    | X.StartTag (name, attrs, selfclosing) ->
      let qname, lnss, attrs =
        Xml.parse_element_head namespaces name attrs in
      let el = (qname, attrs, []) in
        if selfclosing then (
          Xml.remove_namespaces namespaces lnss;
          if Stack.is_empty stack then (
            Stack.push el stack;
            next_token ()
          ) else (
            add_element (Xml.Xmlelement el);
            Xml.remove_namespaces namespaces lnss;
            next_token ()
          )
        ) else (
          Stack.push el stack;
          Stack.push lnss stack_ns;
          next_token ()
        )
    | X.EndTag _name ->
      let lnss = Stack.pop stack_ns in
        Xml.remove_namespaces namespaces lnss;
        if Stack.length stack > 1 then
          let q, a, els = Stack.pop stack in
            add_element (Xml.Xmlelement (q, a, List.rev els));
            next_token ()
        else
          next_token ()
    | X.Text text ->
      add_element (Xml.Xmlcdata text);
      next_token ()
    | X.Doctype _              
    | X.PI _ ->
      next_token ()
  and loop = function
    | Return (Some result) -> loop (process_token result)
    | Return None -> ()
    | Continue cont ->
      if source.i < source.len then
        ()
      else
        let size = input inc source.buf 0 8192 in
          if size = 0 then
            source.is_final <- true
          else (
            source.i <- 0;
            source.len <- size;
          );
          loop (cont source)
  in
    try
      loop (next_token ());
      let (q, a, els) = Stack.pop stack in
        Xml.Xmlelement (q, a, List.rev els)
    with LocatedStream.Located_exn ((line, col), exn) ->
      match exn with
        | M.Exn_msg msg ->
          Printf.eprintf "%d:%d %s\n" line col msg;
          Pervasives.exit 127
        | M.Exn_ExpectedChar chs ->
          Printf.eprintf "%d:%d Expected '%s'\n" line col
            (String.make 1 (List.hd chs));
          Pervasives.exit 127          
        | M.Exn_CharToken u ->
          let chs = M.E.encode_unicode u in
          let str = String.create (List.length chs) in
          let rec iteri i = function
            | [] -> ()
            | x :: xs -> str.[i] <- x; iteri (succ i) xs
          in
            iteri 0 chs;
            Printf.eprintf "%d:%d Unexpected character token %S\n" line col str;
            Pervasives.exit 127
        | exn ->
          Printf.eprintf "%d:%d %s\n" line col (Printexc.to_string exn);
          Pervasives.exit 127

  
