(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml

let unknown_encoding_handler encoding =
  Printf.printf "make_decoder %s\n" encoding;
  Conversion.make_decoder encoding
    
let print_result xml =
  let ser = Xml.Serialization.create [] in
  let buf = Buffer.create 80 in
  let out str =
    if Buffer.length buf >= 80 then (
      Buffer.output_buffer stdout buf;
      Buffer.reset buf
    );
    Buffer.add_string buf str
  in
    Xml.Serialization.serialize_document ser out xml;
    Buffer.output_buffer stdout buf
      
let _ =
  let file = Sys.argv.(1) in
  let f_in = open_in file in
  let buf = Buffer.create 8126 in
    (try while true do
       Buffer.add_string buf (input_line f_in)
     done
     with End_of_file -> close_in f_in);
    let doc = parse_document (Buffer.contents buf) in
      print_result doc
    
