(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

open Xmllexer_generic

module IterMonad =
struct
  type input = {
    buf : string;
    i : int;
    len : int;
  }

  type data =
    | Chunk of input
    | EOF

  let empty_chunk = {
    buf = "";
    i = 0;
    len = 0;
  }

  let empty_stream = Chunk empty_chunk

  let is_empty s =
    s.i = s.len

  type 'a t =
    | Return of 'a
    | Continue of (data -> 'a t * data)

  let return x = Return x
  let fail = raise
  let rec bind v f =
    match v with
      | Return x -> f x
      | Continue k -> Continue (fun s ->
        match k s with
          | Return x, s' -> (
            match f x with
              | Continue k -> k s'
              | i -> (i, s')
          )
          | (i, s') -> (bind i f), s'
      )
    
  let (>>=) = bind

  let rec get =
    Continue (function
      | EOF -> Return None, EOF
      | Chunk s ->
        if s.i < s.len then
          Return (Some s.buf.[s.i]), Chunk {s with i = s.i + 1}
        else
          get, (Chunk s)
    )
end

module Decoder (I : sig type 'a t
                        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
                        val return : 'a -> 'a t
                        val fail : exn -> 'a t
                        type stream
                        val get : char option t end) =
struct
  open I

  exception IllegalCharacter
    
  let decode_utf8 =
    I.get >>= function
      | None -> return None
      | Some ch1 ->
        match ch1 with
          | '\000'..'\127' -> return (Some (Char.code ch1))
          | '\192'..'\223' -> (
            I.get >>= function
              | None -> fail IllegalCharacter
              | Some ch2 ->
                let n1 = Char.code ch1 in
                let n2 = Char.code ch2 in
                  if (n2 lsr 6 != 0b10) then fail IllegalCharacter
                  else
                    let code = ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
                      return (Some (code))
          )
          | '\224'..'\239' -> (
            I.get >>= function
              | None -> fail IllegalCharacter
              | Some ch2 ->
                I.get >>= function
                  | None -> fail IllegalCharacter
                  | Some ch3 ->
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
                          else return (Some (code))
          )
          | '\240'..'\247' -> (
            I.get >>= function
              | None -> fail IllegalCharacter
              | Some ch2 ->
                I.get >>= function
                  | None -> fail IllegalCharacter
                  | Some ch3 ->
                    I.get >>= function
                      | None -> fail IllegalCharacter
                      | Some ch4 ->
                        let n1 = Char.code ch1
                        and n2 = Char.code ch2
                        and n3 = Char.code ch3
                        and n4 = Char.code ch4 in
                          if (n2 lsr 6 != 0b10) ||
                            (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                            fail IllegalCharacter
                          else
                            return (Some (((n1 land 0x07) lsl 18) lor
                                             ((n2 land 0x3f) lsl 12) lor
                                             ((n3 land 0x3f) lsl 6)
                                          lor (n4 land 0x3f)))
          )
          | _ ->
            fail IllegalCharacter
end

module Input =
struct
  module D = Decoder (struct include IterMonad
                             type stream = IterMonad.data
  end)

  exception UnknownEncoding
    
  let make_decoder encname =
    if encname = "UTF-8" then
      D.decode_utf8
    else
      raise UnknownEncoding
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

  type stream = {
    mutable line : int;
    mutable col : int;
    mutable decoder : int option t;
  }

  open Input

  let set_decoder encname strm =
    let decoder = make_decoder encname in
      strm.decoder <- decoder;
      ()

  type source = unit

  let make_stream () =
    { line = 0;
      col = 0;
      decoder = make_decoder "UTF-8";
    }

  let error ?stream exn =
    match stream with
      | None -> fail exn
      | Some strm -> fail (Located_exn ((strm.line, strm.col), exn))

  let next_char strm eof f =
    strm.decoder >>= function
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
  let stream = LocatedStream.make_stream () in
  let buf = String.create 8192 in
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
  and loop v =
    match v with
      | Return (Some result), s -> loop ((process_token result), s)
      | Return None, s -> ()
      | Continue cont, s ->
        let s =
          match s with
            | EOF -> EOF
            | Chunk s ->
              if is_empty s then
                let size = input inc buf 0 8192 in
                  if size = 0 then
                    EOF
                  else
                    let str =
                      if size < 8192 then
                        String.sub buf 0 size
                      else
                        buf
                    in
                      Chunk {buf = str; i = 0; len = size}
              else
                Chunk s
        in
          loop (cont s)
  in
    try
      loop (next_token (), empty_stream);
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

  
