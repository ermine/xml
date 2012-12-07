(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

open Xmllexer_generic

module type INPUT =
sig
  type 'a t
  type stream
  val make_decoder : string -> (stream -> int option t)
end

module UnitMonad =
struct
  type 'a t = 'a
  let return x = x
  let fail = raise
  let (>>=) v f = f v
end

module Decoder (I : sig type 'a t
                        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
                        val return : 'a -> 'a t
                        val fail : exn -> 'a t
                        type stream
                        val get : stream -> char option t end) =
struct
  open I

  exception IllegalCharacter
    
  let decode_utf8 strm =
    I.get strm >>= function
      | None -> return None
      | Some ch1 ->
        match ch1 with
          | '\000'..'\127' -> return (Some (Char.code ch1))
          | '\192'..'\223' -> (
            I.get strm >>= function
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
            I.get strm >>= function
              | None -> fail IllegalCharacter
              | Some ch2 ->
                I.get strm >>= function
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
            I.get strm >>= function
              | None -> fail IllegalCharacter
              | Some ch2 ->
                I.get strm >>= function
                  | None -> fail IllegalCharacter
                  | Some ch3 ->
                    I.get strm >>= function
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

module Input (M : MONAD) =
struct
  type 'a t = 'a M.t
  type stream = char Stream.t
  let get s =
    match Stream.peek s with
      | Some c -> Stream.junk s; M.return (Some c)
      | None -> M.return None

  module D = Decoder 
    (struct include M
            type stream = char Stream.t
            let get = get end)

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

module LocatedStream (M: MONAD) (I : INPUT with type 'a t = 'a M.t) =
struct
  include M
  open I

  type source = I.stream

  exception Located_exn of (int * int) * exn

  type stream = {
    mutable line : int;
    mutable col : int;
    mutable decoder : source -> int option t;
    stream : source
  }

  let set_decoder encname strm =
    let decoder = I.make_decoder encname in
      strm.decoder <- decoder;
      ()

  let make_stream source =
    { line = 0;
      col = 0;
      decoder = I.make_decoder "UTF-8";
      stream = source
    }

  let error ?stream exn =
    match stream with
      | None -> fail exn
      | Some strm -> M.fail (Located_exn ((strm.line, strm.col), exn))

  let next_char strm eof f =
    strm.decoder strm.stream >>= function
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

module LS = LocatedStream(UnitMonad) (Input (UnitMonad))
module M = Make
  (LS)
  (Encoding)
  (XmlStanza (UnitMonad))

module X = XmlStanza (UnitMonad)
open Xml
      
let parse_document inc =
  let strm = LS.make_stream (Stream.of_channel inc) in
  let next_token = M.make_lexer strm in
  let namespaces = Hashtbl.create 1 in
  let () = Hashtbl.add namespaces "xml" ns_xml in
  let stack = Stack.create () in
  let add_element el =
    let (qname, attrs, subels) = Stack.pop stack in
      Stack.push (qname, attrs, (el :: subels)) stack
  in
  let rec loop () =
    match next_token () with
      | Some t -> (
        match t with
          | X.StartTag (name, attrs, selfclosing) ->
            let qname, lnss, attrs = parse_element_head namespaces name attrs in
            let el = (qname, attrs, []) in
              if selfclosing then (
                remove_namespaces namespaces lnss;
                if Stack.is_empty stack then (
                  Stack.push el stack;
                  loop ();
                ) else (
                  add_element (Xmlelement el);
                  loop ()
                )
              ) else (
                Stack.push el stack;
                loop ();
                remove_namespaces namespaces lnss;
                loop ()
              )
          | X.EndTag _name ->
            (* let qname = parse_qname namespaces (split_name name) in *)
            if Stack.length stack > 1 then
              let q, a, els = Stack.pop stack in
                add_element (Xmlelement (q, a, List.rev els))
            else
              ()
          | X.Text text ->
            add_element (Xmlcdata text);
            loop ()
          | X.Doctype _              
          | X.PI _ ->
            loop ()
      )
      | None -> ()
  in
    try
      loop ();
      let (q, a, els) = Stack.pop stack in
        Xmlelement (q, a, List.rev els)
    with LS.Located_exn ((line, col), exn) ->
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
