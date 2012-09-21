(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

module StringStream =
struct
  type 'a t = 'a Xmllexer.UnitMonad.t
  type orig_stream = {
    mutable i : int;
    len : int;
    buf : string
  }

  let of_string str =
    { buf = str; len = String.length str; i = 0}

  let peek s =
    if s.i < s.len then
      Some s.buf.[s.i]
    else
      None
    
  let junk s =
    if s.i < s.len then s.i <- s.i + 1
end

module IconvEncoding =
struct
  open StringStream
  type orig_stream = StringStream.orig_stream
  type 'a t = 'a Xmllexer.UnitMonad.t

  module E = Xmllexer.Encoding(Xmllexer.UnitMonad) (StringStream)
  open Xmllexer.UnitMonad

  let encode_unicode = E.encode_unicode
  let decode_utf8 = E.decode_utf8

  open Encoding

  let make_decoder encname =
    let decoder = decoder encname in
      fun strm ->
        match decode decoder strm.buf strm.i (strm.len - strm.i) with
          | Dec_ok (ucs4, j) ->
            strm.i <- strm.i+j;
            return (Some ucs4)
          | Dec_need_more -> fail E.IllegalCharacter
          | Dec_error -> fail E.IllegalCharacter
    
end

module XmlStanza (M : Xmllexer_generic.Monadable) =
struct
  type 'a t = 'a M.t

  type token = unit

  let emit_start_tag name attrs selfclosing =
    Printf.printf "<%s" name;
    List.iter (fun (k,v) -> Printf.printf " %s='%s'" k v) attrs;
    Printf.printf ">";
    if selfclosing then
      Printf.printf "</%s>" name

  let emit_end_tag name =
    Printf.printf "</%s>" name

  let emit_doctype doctype =
    ()
      
  let emit_pi target data =
    Printf.printf "<?%s %s?>" target data

  let emit_text text =
    Printf.printf "%s" text

  let emit_eof () =
    raise End_of_file
end

open Xmllexer

module M = Xmllexer_generic.Make (Xmllexer_generic.XName)
  (UnitMonad)
  (LikeLWT (UnitMonad) (StringStream))
  (IconvEncoding)
  (XmlStanza (UnitMonad))

let _ =
  let f = open_in Sys.argv.(1) in
  let rec read_file () =
    let line = try Some (input_line f) with End_of_file -> None in
      match line with
        | Some line -> line ^ "\n" ^ read_file ()
        | None -> ""
  in
  let content = read_file () in
  let strm = StringStream.of_string content in
  let next_token = M.make_lexer strm in
  let rec loop () = next_token (); loop () in loop ()
