(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml

let split_attrs attrs =
   List.fold_left (fun (nss, attrs) (name, value) ->
		      let prefix, lname = Xmlparser.split_name name in
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
	   raise (Xml.Error "Root node cannot be bound to any parent")

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


let reparent refel node =
   match refel with 
      | Element (elemp, name, nss, attrs, children) ->
           bind_child refel node;
           (match node with
	       | Attribute (p, name, value) ->
		    attrs := node::!attrs;
               | Text (p, str) ->
		    children := node::!children;
               | Element (p, name, nss, attrs, ch) ->
		    children := node::!children;
               | Comment (p, str) ->
		    children := node::!children;
               | ProcessingInstruction (p, pi, str) ->
		    children := node::!children;
               | NS (p, ns) -> 
		    nss := node::!nss
               | Root (nodes) -> 
		    failwith 
		       "Element node cannot have a root node in its children"
	   )
      | Root (oldnodes) ->
	   bind_child refel node;
	   (match node with
		  Text (p, str) ->
		     oldnodes := node::!oldnodes
	       | Element (p, name, nss, attrs, ch) ->
		    oldnodes := node::!oldnodes
	       | Comment (p, str) ->
		    oldnodes := node::!oldnodes
	       | ProcessingInstruction (p, pi, str) ->
		    oldnodes := node::!oldnodes
	       | _ -> 
		    failwith "Cannot make node a children of root"
	   )
      | _ -> 
	   failwith "Node cannot be a parent of any other"

let reparent_nodes refel nodes =
   List.iter (reparent refel) (List.rev nodes)

let create_dom ?(whitespace_preserve=false) 
      ~unknown_encoding_handler ~entity_handler 
      ~callback () =

   let namespaces = Hashtbl.create 1 in
   let root = Root(ref []) in
   let parse_attributes = 
      List.map (fun (qname,value) -> Attribute (null_noderef (), qname, value))
   in
   let parse_namespaces = 
      List.map (fun map -> NS (null_noderef (), map))
   in
   let new_element qname namespaces attributes children =
      let el = Element (null_noderef (), 
			qname, 
			ref namespaces, 
			ref attributes, 
			ref children)
      in
	 (match el with
             | Element (newp, nname, nnss, nattrs, nch) -> 
		  bind_children el !nnss;
		  bind_children el !nattrs;
		  bind_children el !nch;
	     | _ -> ()
	 );
	 el
   in
   let new_comment str =
      Comment (null_noderef (), str)
   in
   let new_pi name str =
      ProcessingInstruction (null_noderef (), name, str)
   in
   let new_text str =
      Text (null_noderef (), str)
   in
   let rec process_epiloque nodes tag fparser =
      match tag with
	 | Xmlparser.Comment comment ->
	      let node = new_comment comment in
		 fparser (process_epiloque (node :: nodes))
	 | Xmlparser.Pi (target, data) ->
	      let node = new_pi target data in
		 fparser (process_epiloque (node :: nodes))
	 | Xmlparser.Whitespace spaces ->
	      if whitespace_preserve then
		 let node = new_text spaces in
		    fparser (process_epiloque (node :: nodes))
	      else
		 fparser (process_epiloque nodes)
	 | Xmlparser.EOD ->
	      reparent_nodes root (List.rev nodes);
	      callback root
	 | _ ->
	      failwith "Invalid epiloque"
   in
   let rec get_node qname nodes nextf tag fparser =
      match tag with
	 | Xmlparser.Comment comment ->
	      let node = new_comment comment in
		 fparser (get_node qname (node :: nodes) nextf)
	 | Xmlparser.Pi (target, data) ->
	      let node = new_pi target data in
		 fparser (get_node qname (node :: nodes) nextf)
	 | Xmlparser.Whitespace spaces ->
	      if whitespace_preserve then
		 let node = new_text spaces  in
		    fparser (get_node qname (node :: nodes) nextf)
	      else
		 fparser (get_node qname nodes nextf)
	 | Xmlparser.Text str ->
	      let node = new_text str in
		 fparser (get_node qname (node :: nodes) nextf)
	 | Xmlparser.StartElement (name, attrs) ->
	      let lnss, attrs = split_attrs attrs in
		 add_namespaces namespaces lnss;
		 let attrs =  parse_attrs namespaces attrs in
		 let qname' = parse_qname namespaces 
		    (Xmlparser.split_name name) in
		 let newnextf childs fparser =
		    let node = new_element qname'
		       (parse_namespaces lnss) 
		       (parse_attributes attrs) 
		       (List.rev childs)
		    in
		       remove_namespaces namespaces lnss;
		       fparser (get_node qname (node :: nodes) nextf)
		 in
		    fparser (get_node qname' [] newnextf)
	 | Xmlparser.EndElement name ->
	      let qname' = parse_qname namespaces 
		 (Xmlparser.split_name name) in
		 if qname = qname' then
		    nextf nodes fparser
		 else 
		    failwith (Printf.sprintf "Bad end tag: expected %s, was %s"
				 (Xml.string_of_qname qname)
				 (Xml.string_of_qname qname'))
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let lnss, attrs = split_attrs attrs in
		 add_namespaces namespaces lnss;
		 let attrs =  parse_attrs namespaces attrs in
		 let qname' = parse_qname namespaces 
		    (Xmlparser.split_name name) in
		 let node = new_element qname'
		    (parse_namespaces lnss) 
		    (parse_attributes attrs)
		    [] 
		 in
		    remove_namespaces namespaces lnss;
		    fparser (get_node qname (node :: nodes) nextf)
	 | Xmlparser.Cdata cdata -> 
	      let node = new_text cdata in
		 fparser (get_node qname (node :: nodes) nextf)
	 | Xmlparser.Doctype (qname, ext, str) -> 
              failwith "Doctype declaration inside of element"
	 | Xmlparser.EOD ->
	      failwith "Unexpected end of data"
   in
   let rec process_prolog nodes tag fparser =
      match tag with
	 | Xmlparser.Comment comment ->
	      let node = new_comment comment in
		 fparser (process_prolog (node :: nodes))
	 | Xmlparser.Doctype (name, ext_id, str) ->
	      (* todo *)
	      fparser (process_prolog nodes)
	 | Xmlparser.StartElement (name, attrs) ->
	      let lnss, attrs = split_attrs attrs in
		 add_namespaces namespaces lnss;
		 let attrs =  parse_attrs namespaces attrs in
		 let qname = parse_qname namespaces 
		    (Xmlparser.split_name name) in
		 let newnextf childs fparser =
		    let node = new_element qname 
		       (parse_namespaces lnss) 
		       (parse_attributes attrs) 
		       (List.rev childs)
		    in
		       remove_namespaces namespaces lnss;
		       fparser (process_epiloque (node :: nodes))
		 in
		    fparser (get_node qname [] newnextf)
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let lnss, attrs = split_attrs attrs in
		 add_namespaces namespaces lnss;
		 let attrs =  parse_attrs namespaces attrs in
		 let qname = parse_qname namespaces 
		    (Xmlparser.split_name name) in
		 let node = new_element qname 
		    (parse_namespaces lnss) 
		    (parse_attributes attrs)
		    [] 
		 in
		    remove_namespaces namespaces lnss;
		    fparser (process_epiloque (node :: nodes))
	 | Xmlparser.Whitespace spaces ->
	      if whitespace_preserve then
		 let node = new_text spaces in
		    fparser (process_prolog (node :: nodes))
	      else
		 fparser (process_prolog nodes)
	 | Xmlparser.EOD ->
	      failwith "Unexpected end of data"
	 | _ ->
	      failwith "Unexpected tag"
   in      
      Xmlparser.create 
	 ~process_unknown_encoding:unknown_encoding_handler
	 ~process_entity:entity_handler
	 ~process_production:(process_prolog [])
	    ()

let parse = Xmlparser.parse
let set_callback = Xmlparser.set_callback
let finish = Xmlparser.finish
let reset = Xmlparser.reset
