open Light_xml

let unknown_encoding_handler encoding =
   Printf.printf "make_decoder %s\n" encoding;
   Conversion.make_decoder encoding

let callback xml =
   print_endline "parsed"

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
	    parse p s 0 size;
	    aux_read ()
	 )
   in
      aux_read ()
