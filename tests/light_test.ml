(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

let _ = Printexc.record_backtrace true

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
  let strm = Stream.of_channel (open_in Sys.argv.(1)) in
  let doc = parse_document strm in
    print_result doc
    
