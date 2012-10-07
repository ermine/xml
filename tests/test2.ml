(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

module StringStream =
struct
  type 'a t = 'a
  let return x = x
  let fail = raise
    
  type stream = {
    mutable i : int;
    len : int;
    buf : string
  }

  let of_string str =
    { buf = str; len = String.length str; i = 0}

  let get s =
    if s.i < s.len then (
      let c = s.buf.[s.i] in
        s.i <- s.i + 1;
        Some c
    )
    else
      None

  open Encoding
  exception IllegalCharacter
    
  let make_decoder encname =
    let decoder = decoder encname in
      fun strm ->
        if strm.i < strm.len then
          match decode decoder strm.buf strm.i (strm.len - strm.i) with
            | Dec_ok (ucs4, j) ->
              strm.i <- strm.i + j;
              return (Some ucs4)
            | Dec_need_more -> fail IllegalCharacter
            | Dec_error -> fail IllegalCharacter
        else
          return None
end

module XStanza =
struct
  type 'a t = 'a

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

module M = Xmllexer_generic.Make
  (LocatedStream (UnitMonad) (StringStream))
  (Encoding)
  (XStanza)

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
  let strm = M.S.make_stream strm in
  let next_token = M.make_lexer strm in
  let rec loop () = next_token (); loop () in
  let _ = loop () in
    ()
