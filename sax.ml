(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
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
    
  let rec process_production (tag, state) =
    match tag with
      | Pi (target, data) ->
          pi_handler target data;
          process_production (parse state)
      | Comment comment ->
          comment_handler comment;
          process_production (parse state)
      | Cdata cdata ->
          character_data_handler cdata;
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
      | EmptyElement (name, attrs) ->
          start_element_handler name attrs;
          end_element_handler name;
          process_production (parse state)
      | Doctype _dtd ->
          failwith "Unexpected doctype"
      | EndOfBuffer ->
          process_production, state
      | EndOfData ->
          raise End_of_file

  and process_epilogue (tag, state) =
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
          process_epilogue, state
      | EndOfData ->
          raise End_of_file
      | _ ->
          failwith "Unexpected tag in epilogue"
          
  and process_prolog (tag, state) =
   match tag with
      | Comment comment ->
          comment_handler comment;
          process_prolog (parse state)
      | Doctype _dtd ->
          process_prolog (parse state)
      | StartElement (name, attrs) ->
          process_production (tag, state)
      | Whitespace space ->
          process_prolog (parse state)
      | EmptyElement (name, attrs) ->
          start_element_handler name attrs;
          end_element_handler name;
          process_epilogue (parse state)
      | Pi (target, data) ->
          pi_handler target data;
          process_prolog (parse state)
      | EndElement tag ->
          failwith ("Unexpected </" ^ tag ^ ">")
      | Cdata _ ->
          failwith "Unexpected cdata"
      | Text _ ->
          failwith "Unexpected text"
      | EndOfBuffer ->
          process_prolog, state
      | EndOfData ->
          raise End_of_file
  in
  let state = Xmlparser.create
    ?encoding ?unknown_encoding_handler ?entity_resolver () in
    (process_prolog, state) 
      
let parse ?buf ?finish (callback, state) =
  callback (Xmlparser.parse ?buf ?finish state)

let reset (_, state) = Xmlparser.reset state
