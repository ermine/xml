(*
 * (c) 2007-2012 Anastasia Gornostaeva
 *)

let _ = Printexc.record_backtrace true

open Light_xml

let print_result xml =
  let buf = Buffer.create 80 in
(*    
  let out str =
    if Buffer.length buf >= 80 then (
      Buffer.output_buffer stdout buf;
      Buffer.reset buf
    );
    Buffer.add_string buf str
*)
  let out = print_string 
  in
    Serialization.serialize_document out xml;
    Buffer.output_buffer stdout buf
      
let _ =
  let doc = parse_document (open_in Sys.argv.(1)) in
    print_result doc
    
