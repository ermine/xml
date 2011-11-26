(*
 * (c) 2007-2009 Anastasia Gornostaeva
 *)

open Xml

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
  let str = String.create 1024 in
  let rec read_file () =
    let size = input f_in str 0 1024 in
      if size = 0 then (
        close_in f_in;
        Buffer.contents buf
      )
      else (
        Buffer.add_string buf (String.sub str 0 size);
        read_file ()
      )
  in
  let data = read_file () in
  let doc = parse_document data in
    print_result doc
    
