(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

module XmlStanza =
struct
  type 'a t = 'a Xmllexer.LikeLWT.t

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

module M = Xmllexer_generic.Make (Xmllexer_generic.XName)
  (Xmllexer.LikeLWT) (XmlStanza)

let _ =
  let strm = Stream.of_channel (open_in Sys.argv.(1)) in
  let next_token = M.make_lexer strm in
  let rec loop () = next_token (); loop () in loop ()
