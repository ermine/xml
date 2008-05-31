(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml

let unknown_encoding_handler encoding =
   Printf.printf "make_decoder %s\n" encoding;
   Conversion.make_decoder encoding

let callback xml =
   print_endline "parsed";
   let ser = Xml.Serialization.init [] in
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
   let p = create_parser ~unknown_encoding_handler callback in
   let s = String.create 1024 in
   let rec aux_read () =
      let size = input f_in s 0 1024 in
	 if size = 0 then
	    finish p
	 else (
	    parse p s size;
	    aux_read ()
	 )
   in
      aux_read ()
