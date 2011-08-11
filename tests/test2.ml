(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Printf
open Xml

let string_of_ns = function
  | Some uri -> Printf.sprintf "URI %S" uri
  | None -> "None"

let start_ns_handler (ns, prefix) =
  printf "NS start %s %s\n" prefix (string_of_ns ns)
    
let end_ns_handler (ns, prefix) =
  printf "NS end %s %s\n" prefix (string_of_ns ns)
    
let start_element_handler (ns, lname) attrs =
  printf "StartElement (%s) %s\n" (string_of_ns ns) lname;
  List.iter (fun ((ns, lname), value) ->
               Printf.printf "   (%s) %s='%s'\n" (string_of_ns ns) lname value)
    attrs
    
let end_element_handler (ns, lname) =
  printf "EndElement (%s) %s\n" (string_of_ns ns) lname
    
let character_data_handler cdata =
  printf "Cdata %S\n" cdata

let comment_handler comment =
  printf "Comment %s\n" comment
    
let pi_handler target data =
  printf "Pi %s %s\n" target data
    
let unknown_encoding_handler encoding =
  let decoder = Encoding.decoder encoding in
    fun str i ->
      match Encoding.decode decoder str i (String.length str - i) with
        | Encoding.Dec_ok (ucs4, j) -> Xmlencoding.Result ((i+j), ucs4)
        | Encoding.Dec_need_more -> Xmlencoding.TooFew
        | Encoding.Dec_error -> Xmlencoding.Invalid
  
let entity_resolver entity =
  failwith (sprintf "Unknown entity: %s" entity)
    
let _ =
  let fin = open_in Sys.argv.(1) in
  let state = Sax_ns.create
    ~start_ns_handler
    ~end_ns_handler
    ~start_element_handler
    ~end_element_handler
    ~character_data_handler
    ~comment_handler
    ~pi_handler
    ~unknown_encoding_handler
    ~entity_resolver
    ~whitespace_preserve:true () in
    
  let buf = String.create 1024 in
  let rec loop state =
    let size = input fin buf 0 70 in
      if size = 0 then (
        close_in fin;
        ignore (Sax_ns.parse ~finish:true state)
      )
      else (
        print_endline ("[" ^ String.sub buf 0 size ^ "]");
        let state = Sax_ns.parse ~buf:(String.sub buf 0 size) state in
          loop state
      )
  in
    loop state
      
