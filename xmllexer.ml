(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

module Encoding (S : sig
  type 'a t
  type stream
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val peek : stream -> char option t
  val junk : stream -> unit t
    end) =
struct
  open S

  exception IllegalCharacter

  let encode_utf8 ucs4 =
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
        
  let decode_utf8 strm =
    peek strm >>= function
      | None -> return None
      | Some ch1 ->
        junk strm >>= fun () ->
        match ch1 with
          | '\000'..'\127' -> return (Some (Char.code ch1, [ch1]))
          | '\192'..'\223' -> (
            peek strm >>= function
              | None -> return None
              | Some ch2 ->
                junk strm >>= fun () ->
                let n1 = Char.code ch1 in
                let n2 = Char.code ch2 in
                  if (n2 lsr 6 != 0b10) then fail IllegalCharacter
                  else
                    let code = ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
                      return (Some (code, [ch1; ch2]))
          )
          | '\224'..'\239' -> (
            peek strm >>= function
              | None -> return None
              | Some ch2 ->
                junk strm >>= fun () ->
                peek strm >>= function
                  | None -> raise IllegalCharacter
                  | Some ch3 ->
                    junk strm >>= fun () ->
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
                          else return (Some (code, [ch1; ch2; ch3]))
          )
          | '\240'..'\247' -> (
            peek strm >>= function
              | None -> return None
              | Some ch2 ->
                junk strm >>= fun () ->
                peek strm >>= function
                  | None -> raise IllegalCharacter
                  | Some ch3 ->
                    junk strm >>= fun () ->
                    peek strm >>= function
                      | None -> raise IllegalCharacter
                      | Some ch4 ->
                        junk strm >>= fun () ->
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
                                          lor (n4 land 0x3f),
                                          [ch1; ch2; ch3; ch4]))
          )
          | _ ->
            fail IllegalCharacter

end

module LikeLWT =
struct
  type 'a t = 'a

  let return x = x
  let fail = raise
  let (>>=) v f = f v
  let catch f1 f2 = try f1 () with exn -> f2 exn

  exception Located_exn of (int * int) * exn
  let peek = Stream.peek
  let junk = Stream.junk

  type orig_stream = char Stream.t

  type stream = {
    mutable line : int;
    mutable col : int;
    mutable decode : orig_stream -> (int * char list) option;
    stream : char Stream.t
  }
    


  module E = Encoding (struct
    type 'a t = 'a
    let return = return
    let (>>=) = (>>=)
    let fail = fail
    type stream = orig_stream
    let peek = peek
    let junk = junk
  end)

  let encode_unicode = E.encode_utf8

  let make_stream strm =
    let decode = (* E.autodetect strm in *) E.decode_utf8 in
      { line = 0;
        col = 0;
        decode = decode;
        stream = strm
      }

  let fail_located strm exn =
    fail (Located_exn ((strm.line, strm.col), exn))

  let next_char strm (eof:unit -> 'a t)  f =
    strm.decode strm.stream >>= function
      | Some (u, chs) ->
        if u = 0x000A then (
          strm.line <- strm.line + 1;
          strm.col <- 0
        ) else
          strm.col <- strm.col + 1;
        f u
      | None -> eof ()

  let raise_located strm exn =
    fail (Located_exn ((strm.line, strm.col), exn))


  let xml_decl ~version ?encoding ?standalone strm =
    print_endline "xml_decl"
    
end

module XmlStanza =
struct
  type 'a t = 'a LikeLWT.t

  type data =
    | StartTag of string * (string * string) list * bool
    | EndTag of string
    | Doctype of Xmllexer_generic.doctype
    | PI of string * string
    | Text of string

  type token = data option

  let emit_start_tag name attrs selfclosing =
      Some (StartTag (name, attrs, selfclosing))

  let emit_end_tag name =
    Some (EndTag name)

  let emit_doctype doctype =
    Some (Doctype doctype)

  let emit_pi target data =
    Some (PI (target, data))

  let emit_text text =
    Some (Text text)

  let emit_eof () =
    None
end

module M = Xmllexer_generic.Make (Xmllexer_generic.XName) (LikeLWT) (XmlStanza)

include M
