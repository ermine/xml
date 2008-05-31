(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlparser

exception Error of string

let create
      ~start_ns_handler
      ~end_ns_handler
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_resolver
      ?(whitespace_preserve=false)
      () =
   let namespaces = Hashtbl.create 1 in
   let stack_ns = Stack.create () in
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
	       let lnss, attrs = split_attrs attrs in
		  add_namespaces namespaces lnss;
		  let attrs =  parse_attrs namespaces attrs in
		  let qname = parse_qname namespaces (split_name name) in
		     Stack.push (qname, lnss) stack_ns;
		     List.iter start_ns_handler lnss;
		     start_element_handler qname attrs;
	  | EmptyElement (name, attrs) ->
	       let lnss, attrs = split_attrs attrs in
		  add_namespaces namespaces lnss;
		  let attrs =  parse_attrs namespaces attrs in
		  let qname = parse_qname namespaces (split_name name) in
		     List.iter start_ns_handler lnss;
		     start_element_handler qname attrs;
		     end_element_handler qname;
		     List.iter end_ns_handler lnss;
		     remove_namespaces namespaces lnss
	  | EndElement name ->
	       let qname' = parse_qname namespaces (split_name name) in
		  end_element_handler qname';
		  let (qname, lnss) = Stack.pop stack_ns in
		     if qname' = qname then (
			List.iter end_ns_handler lnss;
			remove_namespaces namespaces lnss;
		     )
		     else
			let _, expected = qname' in
			   raise (Error (Printf.sprintf 
					 "Bad end element: expected %s, was %s\n"
					 expected name))
	  | Doctype _dtd ->
	       failwith "Unexpected DOCTYPE"
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
	 | Pi (target, data) ->
	      pi_handler target data;
	      fparser process_prolog
	 | StartElement _ ->
	      process_production tag fparser
	 | Whitespace space ->
	      fparser process_prolog
	 | EmptyElement (name, attrs) ->
	      process_production tag fparser
	 | EndOfData ->
	      raise End_of_file
	 | _ ->
	      failwith "Unexpected tag"
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
