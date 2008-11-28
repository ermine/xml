(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Dom
open Printf

let unknown_encoding_handler encoding =
  printf "make_decoder %s\n" encoding;
  Conversion.make_decoder encoding

let entity_resolver entity =
  failwith (sprintf "Unknown entity: %s" entity)

let print_result dom =
   let out = print_string in
      Dom.serialize out dom

let _ =
   let fin = open_in Sys.argv.(1) in

   let p = create () in
   let dom_parse = make_parser ~whitespace_preserve:true
     ~unknown_encoding_handler ~entity_resolver in
   let buf = String.create 1024 in
   let rec loop parse state =
      let size = input fin buf 0 70 in
	      if size = 0 then (
	        close_in fin;
          match parse (Xmlparser.parse ~finish:true state) with
            | Continue (callback, state) ->
                failwith "Not enough data"
            | Result (r, state) ->
                print_result r
	      )
	      else
          match parse (Xmlparser.parse
                             ~buf:(String.sub buf 0 size) state) with
            | Continue (newcallback, state) ->
                loop newcallback state
            | Result (r, state) ->
                print_result r
   in
     loop dom_parse p
         
