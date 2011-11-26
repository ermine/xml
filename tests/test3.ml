(*
 * (c) 2007-2008, Anastasia Gornostaeva
 *)

open Xml
open Dom
open Printf

let unknown_encoding_handler encoding =
  let decoder = Conversion.make_decoder encoding in
    fun str i ->
      match decoder str i with
        | Cs.Shift j -> Xmlencoding.Shift j
        | Cs.Invalid -> Xmlencoding.Invalid
        | Cs.TooFew -> Xmlencoding.TooFew
        | Cs.Result (j, ucs4) -> Xmlencoding.Result (j, ucs4)

let entity_resolver entity =
  failwith (sprintf "Unknown entity: %s" entity)

let print_result dom =
   let out = print_string in
      Dom.serialize out dom

let _ =
   let fin = open_in Sys.argv.(1) in

   let p = create ~unknown_encoding_handler ~entity_resolver () in
   let dom_parse = make_parser ~whitespace_preserve:true in
   let buf = String.create 1024 in
   let rec loop parse state =
      let size = input fin buf 0 70 in
	      if size = 0 then (
	        close_in fin;
          match parse (Xmlparser.parse ~finish:true state) with
            | Continue _ ->
                failwith "Not enough data"
            | Result (_state, r) ->
                print_result r
	      )
	      else
          match parse (Xmlparser.parse
                             ~buf:(String.sub buf 0 size) state) with
            | Continue (state, newcallback) ->
                loop newcallback state
            | Result (_state, r) ->
                print_result r
   in
     loop dom_parse p
         
