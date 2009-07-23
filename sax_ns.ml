(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlparser

exception Error of string

let create
    ?encoding
    ?unknown_encoding_handler
    ?entity_resolver
    ~start_ns_handler
    ~end_ns_handler
    ~start_element_handler
    ~end_element_handler
    ~character_data_handler
    ~comment_handler
    ~pi_handler
    ?(whitespace_preserve=false) () =

  let namespaces = Hashtbl.create 1 in
  let stack_ns = Stack.create () in

  let rec process_production (state, tag) =
    match tag with
       | Pi (target, data) ->
           pi_handler target data;
           process_production (Xmlparser.parse state)
       | Comment comment ->
           comment_handler comment;
           process_production (Xmlparser.parse state)
       | Text text ->
           character_data_handler text;
           process_production (Xmlparser.parse state)
       | Whitespace space ->
           if whitespace_preserve then
             character_data_handler space;
           process_production (Xmlparser.parse state)
       | StartElement (name, attrs) ->
           let lnss, attrs = split_attrs attrs in
             add_namespaces namespaces lnss;
             let attrs =  parse_attrs namespaces attrs in
             let qname = parse_qname namespaces (split_name name) in
               Stack.push (qname, lnss) stack_ns;
               List.iter start_ns_handler lnss;
               start_element_handler qname attrs;
               process_production (Xmlparser.parse state)
       | EndElement name ->
           let qname' = parse_qname namespaces (split_name name) in
             end_element_handler qname';
             let (qname, lnss) = Stack.pop stack_ns in
               if qname' = qname then (
                 List.iter end_ns_handler lnss;
                 remove_namespaces namespaces lnss;
               )
               else (
                 let _, expected = qname' in
                   raise (Error (Printf.sprintf 
                                   "Bad end element: expected %s, was %s\n"
                                   expected name))
               );
               if Stack.is_empty stack_ns then
                 process_epilogue (Xmlparser.parse state)
               else
                 process_production (Xmlparser.parse state)
       | Doctype _dtd ->
           failwith "Unexpected DOCTYPE"
       | EndOfBuffer ->
           (state, process_production)
       | EndOfData ->
           raise End_of_file
             
  and process_epilogue (state, tag) =
    match tag with
      | Comment comment ->
          comment_handler comment;
          process_epilogue (parse state)
      | Pi (target, data) ->
          pi_handler target data;
          process_epilogue (parse state)
      | Whitespace space ->
          if whitespace_preserve then
            character_data_handler space;
          process_epilogue (parse state)
      | EndOfBuffer ->
          (state, process_epilogue)
      | EndOfData ->
          raise End_of_file
      | Doctype _
      | StartElement _
      | EndElement _
      | Text _ ->
          failwith "Unexpected tag in epilogue"
          
  and process_prolog (state, tag) =
    match tag with
      | Comment comment ->
          comment_handler comment;
          process_prolog (Xmlparser.parse state)
      | Doctype _dtd ->
          process_prolog (Xmlparser.parse state)
      | Pi (target, data) ->
          pi_handler target data;
          process_prolog (Xmlparser.parse state)
      | StartElement _ ->
          process_production (state, tag)
      | Whitespace _space ->
          process_prolog (Xmlparser.parse state)
      | EndOfBuffer ->
          (state, process_prolog)
      | EndOfData ->
          raise End_of_file
      | Text _
      | EndElement _ ->
          failwith "Unexpected tag"
  in        
  let state = Xmlparser.create
    ?encoding ?unknown_encoding_handler ?entity_resolver () in
    (state, process_prolog) 
      
let parse ?buf ?finish (state, callback) =
  callback (Xmlparser.parse ?buf ?finish state)
