(*
 * (c) 2007, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Conversion
open Sax

let _ =
   let start_element_handler tag attrs =
      Printf.printf "StartTag %s\n" tag;
      List.iter (fun (k,v) -> Printf.printf "   %s=%s\n" k v) attrs
   in
   let end_element_handler tag =
      Printf.printf "EndTag %s\n" tag;
   in
   let comment_handler comment =
      Printf.printf "Comment %s\n" comment
   in
   let entity_handler name =
      Printf.printf "Entity %s\n" name;
      Uchar.of_char 'A'
   in
   let character_data_handler cdata =
      Printf.printf "Text %s\n" cdata;
   in
   let unknown_encoding_handler encoding =
      Printf.printf "make_decoder %s\n" encoding;
      Conversion.make_decoder encoding
   in
   let pi_handler target data =
      Printf.printf "Pi %s %s\n" target data
   in
   let file = Sys.argv.(1) in
   let p = Sax.create
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_handler
      () in

   let tin = open_in file in
   let rec aux_cycle () =
      let buf = String.create 1024 in
      let size = input tin buf 0 1024 in
	 if size = 0 then (
	    close_in tin;
	    Xmlparser.finish p
	 )
	 else (
	    Xmlparser.parse p buf 0 size;
	    aux_cycle ()
	 )
   in
      aux_cycle ()
		 
