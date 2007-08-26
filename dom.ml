open Xml

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

let null_noderef () = ref (Root (ref []))

let bind_child parent child =
   match child with
      | Attribute (p, _, _)
      | Element (p, _, _, _, _) 
      | Text (p, _) 
      | Comment (p, _)
      | ProcessingInstruction (p, _, _) 
      | NS (p, _) ->
           p := parent
      | Root (nodes) -> 
	   raise (XMLError "Root node cannot be bound to any parent")

let bind_children parent children =
  List.iter (bind_child parent) children;;

let add_child stack newel =
   if Stack.length stack > 0 then
      let el = Stack.pop stack in
	 match el with
            | Element (parent, _qname, _nss, _attrs, els) ->
		 bind_child el newel;
		 els := newel :: !els;
		 Stack.push el stack
	    | Xml.Root nodes ->
		 bind_child el newel;
		 nodes := newel :: !nodes;
		 Stack.push el stack
            | _ -> ()

let create_dom ?(whitespace_preserve=false) 
      ~unknown_encoding_handler ~entity_handler 
      ~callback () =

   let namespaces = Hashtbl.create 1 in
   let stack = Stack.create () in
   let root = Root(ref []) in
   let () = Stack.push root stack in

   let parse_attributes = 
      List.map (fun (qname,value) -> Attribute (null_noderef (), qname, value))
   in
   let parse_namespaces = 
      List.map (fun map -> NS (null_noderef (), map))
   in
   let new_element qname namespaces attributes =
      let el = Element (null_noderef (), 
			qname, 
			ref namespaces, 
			ref attributes, 
			ref [])
      in
	 match el with
            | Element (newp, nname, nnss, nattrs, nch) -> 
		 bind_children el !nnss;
		 bind_children el !nattrs;
		 bind_children el !nch;
		 Stack.push el stack
	    | _ -> ()
   in
   let new_comment str =
      let el = Comment (null_noderef (), str) in
	 add_child stack el
   in
   let new_pi name str =
      let el = ProcessingInstruction (null_noderef (), name, str) in
	 add_child stack el
   in
   let new_text str =
      let el = Text (null_noderef (), str) in
	 add_child stack el
   in

   let next = function
      | Xmlparser.Comment comment ->
	   new_comment comment
      | Xmlparser.Pi (target, data) ->
	   new_pi target data
      | Xmlparser.Whitespace str ->
	   if whitespace_preserve then
	      new_text str
      | Xmlparser.Text str ->
	   new_text str
      | Xmlparser.StartElement (name, attrs) ->
	   let lnss, attrs = split_attrs attrs in
	      add_namespaces namespaces lnss;
	      let attrs =  parse_attrs namespaces attrs in
	      let qname = parse_qname namespaces (split_name name) in
		 new_element qname 
		    (parse_namespaces lnss) 
		    (parse_attributes attrs) 

      | Xmlparser.EndElement name ->
	   let el = Stack.pop stack in
	      (match el with
		  | Element (_p, qname, nss, _attrs, childs) ->
		       let qname2 = parse_qname namespaces (split_name name) in
			  if qname = qname2 then (
			     childs := List.rev !childs;
			     add_child stack el
			  )
			  else
			     failwith "Strange end element";
			  if Stack.length stack = 1 then
			     let root = Stack.pop stack in
				callback root
		  | _ -> ()
 	      )
      | Xmlparser.Cdata cdata -> 
	   new_text cdata
      | Xmlparser.Doctype (qname, ext, str) -> 
	   ()
(*
           failwith "Doctype declaration inside of element"
*)
   in
      Xmlparser.create 
	 ~process_unknown_encoding:unknown_encoding_handler
	 ~process_entity:entity_handler
	 ~process_production:next
	    ()

let parse = Xmlparser.parse
