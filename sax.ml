open Xml

open Xmlparser

let create
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_handler
      ?(whitespace_preserve=false)
      () =
   let process_production = function
      | Pi (target, data) ->
	   pi_handler target data;
      | Comment comment ->
	   comment_handler comment;
      | Cdata cdata ->
	   character_data_handler cdata;
      | Text text ->
	   character_data_handler text;
      | Whitespace space ->
	   if whitespace_preserve then
	      character_data_handler space
      | StartElement (name, attrs) ->
	   start_element_handler name attrs;
      | EndElement name ->
	   end_element_handler name;
      | Doctype (name, ext, str) ->
	   ()
		       
   in
      Xmlparser.create 
	 ~process_unknown_encoding:unknown_encoding_handler
	 ~process_entity:entity_handler
	 ~process_production
	    ()

let parse = Xmlparser.parse
