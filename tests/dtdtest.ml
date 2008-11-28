(*
 * (c) 2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlparser

let print_result data =
  print_endline "Yes!"

let _ =
  let file = Sys.argv.(1) in
  let buf = Buffer.create 8129 in
  let tin = open_in file in
    (try while true do Buffer.add_string buf (input_line tin) done
     with End_of_file -> close_in tin);
    let data = Buffer.contents buf in
    let process_production (tag, state) =
      match tag with
        | Doctype dtd ->
            print_result dtd.dtd_intsubset
        | _ ->
            failwith (string_of_production tag)
    in
      process_production (parse_dtd data)
