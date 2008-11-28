(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Conversion
open Sax

let _ =
  let start_element_handler tag attrs =
    Printf.printf "<%s" tag;
    List.iter (fun (k,v) -> Printf.printf " %s='%s'\ " k v) attrs;
    Printf.printf ">";
  in
  let end_element_handler tag =
    Printf.printf "</%s>" tag;
  in
  let comment_handler comment =
    Printf.printf "<!-- %s -->" comment
  in
  let entity_resolver name =
    Printf.printf "Entity %s" name;
    "[unknown entity]"
  in
  let character_data_handler cdata =
    Printf.printf "%s" cdata;
  in
  let unknown_encoding_handler encoding =
    Printf.printf "make_decoder %s" encoding;
    Conversion.make_decoder encoding
  in
  let pi_handler target data =
    Printf.printf "<?%s %s?>" target data
  in
  let file = Sys.argv.(1) in
  let tin = open_in file in
  let state = Sax.create
    ~unknown_encoding_handler
    ~entity_resolver 
    ~start_element_handler
    ~end_element_handler
    ~character_data_handler
    ~comment_handler
    ~pi_handler
    ~whitespace_preserve:true () in
    
  let rec aux_cycle state =
    let buf = String.create 10 in
    let size = input tin buf 0 10 in
      if size = 0 then (
        close_in tin;
        ignore (Sax.parse ~finish:true state)
      )
      else (
        let state = Sax.parse ~buf:(String.sub buf 0 size)  state in
          aux_cycle state
      )
  in
    aux_cycle state
