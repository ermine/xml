(*
 * (c) 2007, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Dom
open Printf

let unknown_encoding_handler encoding =
   printf "make_decoder %s\n" encoding;
   Conversion.make_decoder encoding

let entity_handler entity =
   failwith (sprintf "Unknown entity: %s" entity)

let callback dom =
   let out = print_string in
      Dom.serialize out dom

let _ =
   let fin = open_in Sys.argv.(1) in

   let p = create_dom ~whitespace_preserve:true
      ~unknown_encoding_handler ~entity_handler ~callback () in
   let buf = String.create 1024 in
   let rec loop () =
      let size = input fin buf 0 70 in
	 if size = 0 then (
	    close_in fin;
	    Sax_ns.finish p
	 )
	 else (
	    Sax_ns.parse p buf 0 size;
	    loop ()
	 )
   in
      loop ()
