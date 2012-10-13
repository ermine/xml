let () =
  let t = Sys.argv.(1) in
  let f = open_in Sys.argv.(2) in
  let xml =
    match t with
      | "s" ->
        Xmllexer.parse_document f
      | "i" ->
        XmllexerI.parse_document f
      | _ -> failwith "unknown option"        
  in
  let ser = Xml.Serialization.create [] in
  let out = print_string in
    Xml.Serialization.serialize_document ser out xml
