open Xml

open Xmlparser

let split_name name =
   if String.contains name ':' then
      let idx = String.index name ':' in
      let prefix = String.sub name 0 idx in
      let lname = 
	 if idx+1 > String.length name then
	    ""
	 else
	    String.sub name (idx+1) (String.length name - (idx+1))
      in
	 prefix, lname
   else
      "", name

let split_attrs attrs =
   List.fold_left (fun (nss, attrs) (name, value) ->
		      let prefix, lname = split_name name in
			 if prefix = "" && lname = "xmlns" then
			    (("", `URI value) :: nss), attrs
			 else if prefix = "xmlns" && lname <> "" then
			    ((lname, `URI value) :: nss) , attrs
			 else
			    nss, (((prefix, lname), value) :: attrs)
		  ) ([], []) attrs

let add_namespaces namespaces nss =
   List.iter (fun (prefix, ns) -> Hashtbl.add namespaces prefix ns) nss

let remove_namespaces namespaces nss =
   List.iter (fun (prefix, ns) -> Hashtbl.remove namespaces prefix) nss

let parse_qname nss (prefix, lname) =
   try
      let ns_mapping = Hashtbl.find nss prefix in
	 (prefix, ns_mapping), lname
   with Not_found ->
      (prefix, `None), lname

let parse_attrs nss attrs =
   List.map (fun (name, value) -> parse_qname nss name, value) attrs

let create
      ~start_ns_handler
      ~end_ns_handler
      ~start_element_handler
      ~end_element_handler
      ~character_data_handler
      ~comment_handler
      ~pi_handler
      ~unknown_encoding_handler
      ~entity_handler
      ?(whitespace_preserve=false)
      () =
   let namespaces = Hashtbl.create 1 in
   let stack_ns = Stack.create () in
   let rec process_production tag fparser =
      (match tag with
	  | Pi (target, data) ->
	       pi_handler target data;
	  | Comment comment ->
	       comment_handler comment;
	  | Cdata cdata ->
	       character_data_handler cdata;
	  | Text text ->
	       character_data_handler text;
	  | Whitespace space ->
	       if whitespace_preserve then
		  character_data_handler space
	  | StartElement (name, attrs) ->
	       let lnss, attrs = split_attrs attrs in
		  add_namespaces namespaces lnss;
		  let attrs =  parse_attrs namespaces attrs in
		  let qname = parse_qname namespaces (split_name name) in
		     Stack.push (qname, lnss) stack_ns;
		     List.iter start_ns_handler lnss;
		     start_element_handler qname attrs;
	  | EmptyElement (name, attrs) ->
	       let lnss, attrs = split_attrs attrs in
		  add_namespaces namespaces lnss;
		  let attrs =  parse_attrs namespaces attrs in
		  let qname = parse_qname namespaces (split_name name) in
		     List.iter start_ns_handler lnss;
		     start_element_handler qname attrs;
		     end_element_handler qname;
		     List.iter end_ns_handler lnss;
		     remove_namespaces namespaces lnss
	  | EndElement name ->
	       let qname = 
		  parse_qname namespaces (split_name name) in
		  end_element_handler qname;
		  let (name, lnss) = Stack.pop stack_ns in
		     if qname = name then (
			List.iter end_ns_handler lnss;
			remove_namespaces namespaces lnss;
		     )
		     else
			failwith (Printf.sprintf 
				     "Bad end element: expected %s, was %s\n"
				     (Xml.string_of_qname name)
				     (Xml.string_of_qname qname));
	  | Doctype (name, ext, str) ->
	       failwith "Unexpected DOCTYPE"
	  | EOD ->
	       raise End_of_file
      );
      fparser process_production
   in
   let rec process_prolog tag fparser =
      match tag with
	 | Comment comment ->
	      comment_handler comment;
	      fparser process_prolog
	 | Doctype (name, ext_id, str) ->
	      fparser process_prolog
	 | StartElement _ ->
	      process_production tag fparser
	 | Whitespace space ->
	      fparser process_prolog
	 | EmptyElement (name, attrs) ->
	      process_production tag fparser
	 | EOD ->
	      raise End_of_file
	 | _ ->
	      failwith "Unexpected tag"
   in	      
      Xmlparser.create 
	 ~process_unknown_encoding:unknown_encoding_handler
	 ~process_entity:entity_handler
	 ~process_production:process_prolog
	    ()

let parse = Xmlparser.parse
let finish = Xmlparser.finish
