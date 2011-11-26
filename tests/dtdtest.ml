(*
 * (c) 2008 Anastasia Gornostaeva
 *)

open Xml
open Xmlparser

let print_result data =
  print_endline "Yes!"

let _ =
  let file = Sys.argv.(1) in
  let f_in = open_in file in
  let buf = Buffer.create 8129 in    
  let str = String.create 1024 in
  let rec read_file () =
    let size = input f_in str 0 1024 in
      if size = 0 then (
        close_in f_in;
        Buffer.contents buf
      )
      else (
        Buffer.add_string buf (String.sub str 0 size);
        read_file ()
      )
  in
  let data = read_file () in
  let process_production (tag, state) =
    match tag with
      | Doctype dtd ->
          print_result dtd.dtd_intsubset
      | _ ->
          failwith (string_of_production tag)
  in
    process_production (parse_dtd data)
