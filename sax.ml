(*
 * (c) 2007-2009 Anastasia Gornostaeva
 *)

open Xml
open Xmlparser

let create
    ?encoding
    ?unknown_encoding_handler
    ?entity_resolver
    ~start_element_handler
    ~end_element_handler
    ~character_data_handler
    ~comment_handler
    ~pi_handler
    ?(whitespace_preserve=false) () =

  let stack = Stack.create () in

  let rec process_production (state, tag) =
    match tag with
      | Pi (target, data) ->
          pi_handler target data;
          process_production (parse state)
      | Comment comment ->
          comment_handler comment;
          process_production (parse state)
      | Text text ->
          character_data_handler text;
          process_production (parse state)
      | Whitespace space ->
          if whitespace_preserve then
            character_data_handler space;
          process_production (parse state)
      | StartElement (name, attrs) ->
          Stack.push name stack;
          start_element_handler name attrs;
          process_production (parse state)
      | EndElement name ->
          let name' = Stack.pop stack in
            if name = name' then
              end_element_handler name
            else
              failwith "Unmatched end tag name";
            if Stack.is_empty stack then
              process_epilogue (parse state)
            else
              process_production (parse state)
      | Doctype _dtd ->
          failwith "Unexpected doctype"
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
      | EndElement _
      | StartElement _
      | Text _->
          failwith "Unexpected tag in epilogue"
    
  and process_prolog (state, tag) =
   match tag with
      | Comment comment ->
          comment_handler comment;
          process_prolog (parse state)
      | Doctype _dtd ->
          process_prolog (parse state)
      | StartElement _ ->
          process_production (state, tag)
      | Whitespace _space ->
          process_prolog (parse state)
      | Pi (target, data) ->
          pi_handler target data;
          process_prolog (parse state)
      | EndElement name ->
          failwith ("Unexpected </" ^ name ^ ">")
      | Text _ ->
          failwith "Unexpected text"
      | EndOfBuffer ->
          (state, process_prolog)
      | EndOfData ->
          raise End_of_file
  in
  let state = Xmlparser.create
    ?encoding ?unknown_encoding_handler ?entity_resolver () in
    (state, process_prolog) 
      
let parse ?buf ?finish (state, callback) =
  callback (Xmlparser.parse ?buf ?finish state)
