(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlparser

let create
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_resolver
      ?(whitespace_preserve=false)
      () =
   let rec process_production tag fparser =
      (match tag with
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
	 | EmptyElement (name, attrs) ->
	      start_element_handler name attrs;
	      end_element_handler name
	 | Doctype _dtd ->
	      failwith "Unexpected doctype"
	 | EndOfData ->
	      raise End_of_file
      );
      fparser process_production
   in
   let rec process_prolog tag fparser =
      match tag with
	 | Comment comment ->
	      comment_handler comment;
	      fparser process_prolog
	 | Doctype _dtd ->
	      fparser process_prolog
	 | StartElement (name, attrs) ->
	      start_element_handler name attrs;
	      fparser process_production
	 | Whitespace space ->
	      fparser process_prolog
	 | EmptyElement (name, attrs) ->
	      start_element_handler name attrs;
	      end_element_handler name
	 | Pi (target, data) ->
	      pi_handler target data;
	      fparser process_prolog
	 | EndElement tag ->
	      failwith ("Unexpected </" ^ tag ^ ">")
	 | Cdata _ ->
	      failwith "Unexpected cdata"
	 | Text _ ->
	      failwith "Unexpected text"
	 | EndOfData ->
	      failwith "Unexpected EOD"
   in
      Xmlparser.create 
	 ~process_unknown_encoding:unknown_encoding_handler
	 ~entity_resolver
	 ~process_production:process_prolog
	    ()

let parse = Xmlparser.parse
let set_callback = Xmlparser.set_callback
let finish = Xmlparser.finish
let reset = Xmlparser.reset
