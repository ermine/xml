(*
 * (c) 2007 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml

exception NonXmlelement

type element = | Xmlelement of qname * ns_mapping list * 
   attribute list * element list
	       | Xmlcdata of cdata

let string_of_attr (qname, value) =
   (string_of_qname qname) ^ "='" ^ Xml.encode value ^ "'"

let string_of_ns (prefix, ns) =
   match ns with 
      | `None -> ""       
      | `URI str ->
           if prefix = "" then
              "xmlns='" ^ Xml.encode str ^ "'"
           else
              "xmlns:" ^ prefix ^ "='" ^ Xml.encode  str ^ "'"

let rec serialize out = function
   | Xmlelement (qname, lnss, attrs, children) ->
	out "<";
        out (string_of_qname qname);
	if attrs <> [] then (
           out " ";
           out (string_of_list string_of_attr " " attrs)
        );
        if lnss <> [] then (
           out " ";
           out (string_of_list string_of_ns " " lnss)
        );
        if children = [] then
           out "/>"
        else (
           out ">";
           List.iter (serialize out) children;
           out "</";
           out (string_of_qname qname);
           out ">"
        )
   | Xmlcdata text ->
        out (Xml.encode text)

let get_tag = function
   | Xmlelement (qname, _, _, _) -> qname
   | _ -> raise NonXmlelement

let get_namespaces = function
   | Xmlelement (_, nss, _, _) -> nss
   | _ -> raise NonXmlelement

let get_namespace ((_prefix, namespace), _name) = namespace

let get_ns_mapping (ns_mapping, _) = ns_mapping

let get_name ((_prefix, _namespace), name) = name

let get_attrs = function
   | Xmlelement (_, _, attrs, _) -> attrs
   | _ -> raise NonXmlelement

let get_attr_value qname attrs =
   let (_, value) = 
      List.find (fun (qname', value) -> equal_qname qname' qname) attrs in
      value

let safe_get_attr_value qname attrs =
   try
      let (_, value) = 
	 List.find (fun (qname', value) -> equal_qname qname' qname) attrs in
	 value
   with Not_found -> ""

let get_element qname childs =
   List.find (function
		 | Xmlelement (qname', _, _, _) -> 
		      equal_qname qname' qname
		 | _ -> false
	     ) childs

let get_elements qname childs =
   List.filter (function
		   | Xmlelement (qname', _, _, _) -> 
			equal_qname qname' qname
		   | Xmlcdata cdata ->
			false
	       ) childs

let get_subelement qname = function
   | Xmlelement (_, _, _, childs) ->
	get_element qname childs
   | _ ->
	raise NonXmlelement

let get_subelements qname = function
   | Xmlelement (_, _, _, childs) ->
	get_elements qname childs
   | _ ->
	raise NonXmlelement

let get_children = function
   | Xmlelement (_, _, _, children) -> children
   | _ -> raise NonXmlelement

let get_first_element els =
   List.find (function
		 | Xmlelement _ -> true
		 | _ -> false) els

let get_cdata = function
   | Xmlelement (_, _, _, els) ->
        let rec collect_cdata els acc =
           match els with
              | (Xmlcdata cdata) :: l -> collect_cdata l (cdata :: acc)
              | _ :: l -> collect_cdata l acc
              | [] -> String.concat "" (List.rev acc)
        in
           collect_cdata els []
   | _ -> raise NonXmlelement

let remove_cdata els =
   List.filter (function
		   | Xmlelement _ -> true
		   | _ -> false) els

let make_element qname nss attrs children =
   Xmlelement (qname, nss, attrs, children)

let make_simple_cdata name cdata =
   Xmlelement ((default_ns, name), [], [], [Xmlcdata cdata])

let mem_qname qname els =
   List.exists (function
		   | Xmlelement (qname', _, _, _) ->
			equal_qname qname' qname
		   | _ -> false) els

let mem_child qname el =
   let childs = get_children el in
      mem_qname qname childs

let iter f el = List.iter f (get_children el)

(*
 * Parsing
 *)

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

let parse_qname_attribute nss (prefix, lname) = 
   if prefix = "" then
      default_ns, lname
   else
      try
	 let ns_mapping = Hashtbl.find nss prefix in
	    (prefix, ns_mapping), lname
      with Not_found ->
	 (prefix, `None), lname

let parse_attrs nss attrs =
   List.map (fun (name, value) -> parse_qname_attribute nss name, value) attrs

let parse_element_head namespaces name attrs =
   let lnss, attrs = split_attrs attrs in
      add_namespaces namespaces lnss;
      let qname = parse_qname namespaces (Xmlparser.split_name name) in
      let attrs =  parse_attrs namespaces attrs in
	 qname, lnss, attrs

let process_production callback =
   let namespaces = Hashtbl.create 1 in

   let rec process_prolog tag fparser =
      match tag with
	 | Xmlparser.Comment _
	 | Xmlparser.Doctype _
	 | Xmlparser.Whitespace _ ->
	      fparser process_prolog
	 | Xmlparser.StartElement (name, attrs) ->
	      let qname, lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let nextf childs fparser =
		 let el = Xmlelement (qname, lnss, attrs, childs) in
		    remove_namespaces namespaces lnss;
		    fparser (process_epiloque el)
	      in
		 fparser (get_childs qname nextf [])
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let qname, lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let el = Xmlelement (qname, lnss, attrs, []) in
		 fparser (process_epiloque el)
	 | Xmlparser.EOD ->
	      raise End_of_file
	 | _ ->
	      failwith "Unexpected tag"
      
   and  get_childs qname nextf childs tag fparser =
      match tag with
	 | Xmlparser.Whitespace str
	 | Xmlparser.Text str
	 | Xmlparser.Cdata str -> 
	      fparser (get_childs qname nextf (Xmlcdata str :: childs))
	 | Xmlparser.StartElement (name, attrs) ->
	      let qname', lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let newnextf childs' fparser =
		 let child = 
		    Xmlelement (qname', lnss, attrs, List.rev childs') in
		    remove_namespaces namespaces lnss;
		    fparser (get_childs qname nextf (child :: childs))
	      in
		 fparser (get_childs qname' newnextf [])
	 | Xmlparser.EndElement name ->
	      let qname' = parse_qname namespaces (Xmlparser.split_name name) in
		 if qname = qname' then
		    nextf childs fparser
		 else 
		    failwith (Printf.sprintf "Bad end tag: expected %s, was %s"
				 (string_of_qname qname)
				 (string_of_qname qname'))
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let qname', lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let child = Xmlelement (qname', lnss, attrs, []) in
		 remove_namespaces namespaces lnss;
		 fparser (get_childs qname nextf (child :: childs))
	 | Xmlparser.Comment _
	 | Xmlparser.Pi _ ->
	      fparser (get_childs qname nextf childs)
	 | Xmlparser.Doctype (qname, ext, str) -> 
              failwith "Doctype declaration inside of element"
	 | Xmlparser.EOD ->
	      raise End_of_file

   and process_epiloque el tag fparser =
      match tag with
	 | Xmlparser.Comment _
	 | Xmlparser.Pi _
	 | Xmlparser.Whitespace _ ->
	      fparser (process_epiloque el)
	 | Xmlparser.EOD ->
	      callback el
	 | _ ->
	      failwith "Invalid epiloque"
   in
      process_prolog


let create_parser ?unknown_encoding_handler callback =
   Xmlparser.create 
      ?process_unknown_encoding:unknown_encoding_handler
      ~process_production:(process_production callback) ()

let parse = Xmlparser.parse

let finish = Xmlparser.finish

let parse_document ?unknown_encoding_handler buf callback =
   let p = Xmlparser.create 
      ?process_unknown_encoding:unknown_encoding_handler
      ~process_production:(process_production callback) () in
      Xmlparser.parse p buf 0 (String.length buf);
      Xmlparser.finish p
