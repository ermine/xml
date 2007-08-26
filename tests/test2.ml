open Printf
open Xml

let start_ns_handler (prefix, ns) 
=   printf "NS start %s %s\n" prefix (match ns with
					 | `URI uri -> "URI " ^ uri
					 | `None -> "None"
				    )

let end_ns_handler (prefix, ns) =
   printf "NS end %s %s\n" prefix (match ns with
				      | `URI uri -> "URI " ^ uri
				      | `None -> "None"
				  )

let start_element_handler qname attrs =
   printf "StartElement %s\n" (string_of_qname qname);
   List.iter (fun (qname, value) ->
		 Printf.printf "   %s='%s'\n" (string_of_qname qname) value)
      attrs

let end_element_handler qname =
   printf "EndElement %s\n" (string_of_qname qname)

let character_data_handler cdata =
   printf "Cdata [%s]\n" cdata

let comment_handler comment =
   printf "Comment %s\n" comment

let pi_handler target data =
   printf "Pi %s %s\n" target data

let unknown_encoding_handler encoding =
   printf "make_decoder %s\n" encoding;
   Conversion.make_decoder encoding

let entity_handler entity =
   failwith (sprintf "Unknown entity: %s" entity)

let _ =
   let fin = open_in Sys.argv.(1) in
   let p = Sax_ns.create
      ~start_ns_handler
      ~end_ns_handler
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_handler
      () in
   let buf = String.create 1024 in
   let rec loop () =
      let size = input fin buf 0 70 in
	 if size = 0 then (
	    close_in fin;
	 )
	 else (
	    Sax_ns.parse p buf 0 size;
	    loop ()
	 )
   in
      loop ()
