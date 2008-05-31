(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 * 
 * http://www.w3.org/TR/xml (fourth edition)
 * http://www.w3.org/TR/REC-xml-names
 *)

exception NonXmlelement
exception InvalidNS

type namespace = [
| 
`URI of string
| `None
]

type prefix = string

type ncname = string

type qname = namespace * ncname

type cdata = string

type attribute = qname * cdata

type element = 
   | Xmlelement of qname * attribute list * element list
   | Xmlcdata of cdata

let ns_xml = `URI "http://www.w3.org/XML/1998/namespace"

let no_ns = `None

let encode = Xml_encode.encode
let decode = Xml_decode.decode

module Serialization =
struct
   type t = {
      default_nss: namespace list;
      bindings: (string, string) Hashtbl.t
   }

   let bind_prefix t prefix namespace =
      match namespace with
	 | `None -> raise InvalidNS
	 | `URI str ->
	      Hashtbl.add t.bindings str prefix

   let init default_nss =
      let bindings = Hashtbl.create 5 in
      let t =
	 { default_nss = default_nss;
	   bindings = bindings
	 } in
	 bind_prefix t "xml" ns_xml;
	 t

   let string_of_qname t (ns, name) =
      match ns with
	 | `None -> name
	 | `URI str -> 
	      let prefix =
		 try Hashtbl.find t.bindings str with Not_found -> "" in
		 if prefix = "" then
		    name
		 else
		    prefix ^ ":" ^ name

   let string_of_attr t (qname, value) =
      (string_of_qname t qname) ^ "='" ^ encode value ^ "'"

   let string_of_list f sep = function
      | [] -> ""
      | x :: [] -> f x
      | x :: xs -> List.fold_left (fun res x -> res ^ sep ^ (f x)) (f x) xs

   let string_of_ns t ns =
      match ns with 
	 | `None -> ""
	 | `URI str ->
	      let prefix = 
		 try Hashtbl.find t.bindings str with Not_found -> "" in
		 if prefix = "" then
		    "xmlns='" ^ encode str ^ "'"
		 else
		    "xmlns:" ^ prefix ^ "='" ^ encode  str ^ "'"


   let local_namespaces t (ns, _name) attrs lnss =
      let lnss =
	 if List.mem ns t.default_nss || List.mem ns lnss then
	    lnss
	 else
	    ns :: lnss
      in
	 List.fold_left (fun acc ((ns, _name), _value) ->
			    if ns = `None ||
			       ns = ns_xml ||
			       List.mem ns t.default_nss || 
			       List.mem ns lnss then
				  acc
			    else
			       ns :: acc) lnss attrs
	 
   let rec aux_serialize lnss t out = function
      | Xmlelement ((ns, _name) as qname, attrs, children) ->
	   out "<";
           out (string_of_qname t qname);
	   if attrs <> [] then (
              out " ";
              out (string_of_list (string_of_attr t) " " attrs)
           );
	   let lnss = local_namespaces t qname attrs lnss in
              if lnss <> [] then (
		 out " ";
		 out (string_of_list (string_of_ns t) " " lnss)
              );
              if children = [] then
		 out "/>"
              else (
		 out ">";
		 List.iter (aux_serialize []
			       {t with default_nss = lnss @ t.default_nss} 
			       out) children;
		 out "</";
		 out (string_of_qname t qname);
		 out ">"
              )
      | Xmlcdata text ->
           out (encode text)

   let serialize_document t out xml =
      aux_serialize t.default_nss t out xml

end

let get_tag = function
   | Xmlelement (qname, _, _) -> qname
   | _ -> raise NonXmlelement

let get_namespace (namespace, _name) = namespace

let get_name (_namespace, name) = name

let get_attrs = function
   | Xmlelement (_, attrs, _) -> attrs
   | _ -> raise NonXmlelement

let get_attr_value qname attrs =
   let (_, value) = List.find (fun (qname', value) -> qname' = qname) attrs in
      value

let safe_get_attr_value qname attrs =
   try get_attr_value qname attrs with Not_found -> ""

let get_element qname childs =
   List.find (function
		 | Xmlelement (qname', _, _) -> qname' = qname
		 | _ -> false
	     ) childs

let get_elements qname childs =
   List.filter (function
		   | Xmlelement (qname', _, _) -> qname' = qname
		   | Xmlcdata cdata -> false
	       ) childs

let get_children = function
   | Xmlelement (_, _, children) -> children
   | _ -> raise NonXmlelement

let get_subelement qname el =
   get_element qname (get_children el)

let get_subelements qname el =
   get_elements qname (get_children el)

let get_first_element els =
   List.find (function
		 | Xmlelement _ -> true
		 | _ -> false) els

let get_cdata el =
   let childs = get_children el in
   let rec collect_cdata acc = function
      | [] -> String.concat "" (List.rev acc)
      | (Xmlcdata cdata) :: l -> collect_cdata (cdata :: acc) l
      | _ :: l -> collect_cdata acc l
   in
      collect_cdata [] childs

let remove_cdata els =
   List.filter (function
		   | Xmlelement _ -> true
		   | _ -> false) els

let make_element qname attrs children =
   Xmlelement (qname, attrs, children)

let make_simple_cdata qname cdata =
   Xmlelement (qname, [], [Xmlcdata cdata])

let mem_qname qname els =
   List.exists (function
		   | Xmlelement (qname', _, _) -> qname' = qname
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
			    ((`URI value, "") :: nss), attrs
			 else if prefix = "xmlns" && lname <> "" then
			    ((`URI value, lname) :: nss) , attrs
			 else
			    nss, (((prefix, lname), value) :: attrs)
		  ) ([], []) attrs

let add_namespaces namespaces nss =
   List.iter (fun (ns, prefix) -> Hashtbl.add namespaces prefix ns) nss

let remove_namespaces namespaces nss =
   List.iter (fun (ns, prefix) -> Hashtbl.remove namespaces prefix) nss

let parse_qname nss (prefix, lname) =
   try
      let namespace = Hashtbl.find nss prefix in
	 (namespace, lname)
   with Not_found ->
      (`None, prefix ^ ":" ^ lname)

let parse_qname_attribute nss (prefix, lname) = 
   if prefix = "" then
      no_ns, lname
   else
      try
	 let ns = Hashtbl.find nss prefix in
	    ns, lname
      with Not_found ->
	 (`None, prefix ^ ":" ^ lname)

let parse_attrs nss attrs =
   List.map (fun (name, value) -> parse_qname_attribute nss name, value) attrs

let parse_element_head namespaces name attrs =
   let lnss, attrs = split_attrs attrs in
      add_namespaces namespaces lnss;
      let qname = parse_qname namespaces (Xmlparser.split_name name) in
      let attrs =  parse_attrs namespaces attrs in
	 qname, lnss, attrs

let string_of_tag (ns, name) =
   let prefix =
      match ns with
	 | `None -> ""
	 | `URI str -> "URI " ^ str
   in
      Printf.sprintf "(%S) %s" prefix name

let process_production callback =
   let namespaces = Hashtbl.create 1 in
   let () = Hashtbl.add namespaces "xml" ns_xml in

   let rec process_prolog tag fparser =
      match tag with
	 | Xmlparser.Comment _
	 | Xmlparser.Doctype _
	 | Xmlparser.Pi _
	 | Xmlparser.Whitespace _ ->
	      fparser process_prolog
	 | Xmlparser.StartElement (name, attrs) ->
	      let qname, lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let nextf childs fparser =
		 let el = Xmlelement (qname, attrs, childs) in
		    remove_namespaces namespaces lnss;
		    fparser (process_epiloque el)
	      in
		 fparser (get_childs qname nextf [])
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let qname, lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let el = Xmlelement (qname, attrs, []) in
		 remove_namespaces namespaces lnss;
		 fparser (process_epiloque el)
	 | Xmlparser.EndOfData ->
	      raise End_of_file
	 | _ ->
	      failwith "Unexpected tag"
      
   and get_childs qname nextf childs tag fparser =
      match tag with
	 | Xmlparser.Whitespace str ->
print_endline ("whitespace [" ^ str ^ "]");
	      fparser (get_childs qname nextf (Xmlcdata str :: childs))
	 | Xmlparser.Text str
	 | Xmlparser.Cdata str -> 
	      fparser (get_childs qname nextf (Xmlcdata str :: childs))
	 | Xmlparser.StartElement (name, attrs) ->
	      let qname', lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let newnextf childs' fparser =
		 let child = 
		    Xmlelement (qname', attrs, childs') in
		    remove_namespaces namespaces lnss;
		    fparser (get_childs qname nextf (child :: childs))
	      in
		 fparser (get_childs qname' newnextf [])
	 | Xmlparser.EndElement name ->
	      let qname' = parse_qname namespaces (Xmlparser.split_name name) in
		 if qname = qname' then
		    nextf (List.rev childs) fparser
		 else 
		    failwith (Printf.sprintf "Bad end tag: expected %s, was %s"
				 (string_of_tag qname)
				 (string_of_tag qname'))
	 | Xmlparser.EmptyElement (name, attrs) ->
	      let qname', lnss, attrs = 
		 parse_element_head namespaces name attrs in
	      let child = Xmlelement (qname', attrs, []) in
		 remove_namespaces namespaces lnss;
		 fparser (get_childs qname nextf (child :: childs))
	 | Xmlparser.Comment _
	 | Xmlparser.Pi _ ->
	      fparser (get_childs qname nextf childs)
	 | Xmlparser.Doctype _dtd ->
              failwith "Doctype declaration inside of element"
	 | Xmlparser.EndOfData ->
	      raise End_of_file

   and process_epiloque el tag fparser =
      match tag with
	 | Xmlparser.Comment _
	 | Xmlparser.Pi _
	 | Xmlparser.Whitespace _ ->
	      fparser (process_epiloque el)
	 | Xmlparser.EndOfData ->
	      callback el
	 | _ ->
	      failwith "Invalid epiloque"
   in
      process_prolog


let create_parser ?unknown_encoding_handler ?entity_resolver callback =
   Xmlparser.create 
      ?process_unknown_encoding:unknown_encoding_handler
      ?entity_resolver:entity_resolver
      ~process_production:(process_production callback) ()

let parse = Xmlparser.parse

let finish = Xmlparser.finish

let parse_document ?unknown_encoding_handler ?entity_resolver
      buf callback =
   let p = Xmlparser.create 
      ?process_unknown_encoding:unknown_encoding_handler
      ?entity_resolver
      ~process_production:(process_production callback) () in
      Xmlparser.parse p buf (String.length buf);
      Xmlparser.finish p
