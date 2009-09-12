(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 * 
 * http://www.w3.org/TR/xml (fourth edition)
 * http://www.w3.org/TR/REC-xml-names
 *)

exception InvalidNS

type namespace = string option

let no_ns = None

let ns_xml = Some "http://www.w3.org/XML/1998/namespace"

type prefix = string

type ncname = string

type name = ncname

type cdata = string

module Attribute =
struct
  type attribute = {
    name : ncname;
    ns : namespace;
    value : string
  }
end

module Element =
struct
  type element = {
    name : ncname;
    ns  : namespace;
    attrs : Attribute.attribute list;
    children : child list
  }
  and child = Element of element | Cdata of cdata
end

let encode = Xml_encode.encode
let decode = Xml_decode.decode

module Serialization =
struct
  module Attr = Attribute
 
  open Element
  
  type t = {
    mutable tmp_prefix: int;
    default_nss: namespace list;
    bindings: (string, string) Hashtbl.t
  }

  let get_default_nss t = t.default_nss

   let bind_prefix t prefix namespace =
     match namespace with
       | None -> raise InvalidNS
       | Some str -> Hashtbl.add t.bindings str prefix
             
   let create default_nss =
     let bindings = Hashtbl.create 5 in
     let t = {
       tmp_prefix = 0;
       default_nss = default_nss;
       bindings = bindings
     } in
       bind_prefix t "xml" ns_xml;
       t

   let string_of_qname t ns name =
     let prefix =
       match ns with
         | None -> ""
         | Some str ->
             try Hashtbl.find t.bindings str with Not_found -> ""
     in
       if prefix = "" then
         name
       else
         prefix ^ ":" ^ name
                 
   let string_of_attr t attr =
     (string_of_qname t attr.Attr.ns attr.Attr.name) ^ "='" ^
       encode attr.Attr.value ^ "'"
       
   let string_of_list f sep = function
     | [] -> ""
     | x :: [] -> f x
     | x :: xs -> List.fold_left (fun res x -> res ^ sep ^ (f x)) (f x) xs
         
   let local_namespaces lnss t el =
     let lnss =
       if List.mem el.ns lnss || List.mem el.ns t.default_nss then
         lnss
       else
         el.ns :: lnss
     in
       List.fold_left (fun acc attr ->
                         if attr.Attr.ns = no_ns ||
                           attr.Attr.ns = ns_xml ||
                           List.mem attr.Attr.ns t.default_nss || 
                           List.mem attr.Attr.ns lnss then
                             acc
                         else
                           match attr.Attr.ns with
                             | None -> acc
                             | Some str ->
                                 if not (Hashtbl.mem t.bindings str) then (
                                   t.tmp_prefix <- t.tmp_prefix + 1;
                                   let p = "ns" ^ string_of_int t.tmp_prefix in
                                     bind_prefix t p attr.Attr.ns;
                                 );
                                 attr.Attr.ns :: acc
                      ) lnss el.attrs
         
   let string_of_ns t = function
     | None ->
         "xmlns=''"
     | Some str ->
         let prefix =
           try Hashtbl.find t.bindings str
           with Not_found -> ""
         in
           if prefix = "" then
             "xmlns='" ^ encode str ^ "'"
           else
             "xmlns:" ^ prefix ^ "='" ^ encode  str ^ "'"

   let rec aux_serialize lnss t out = function
     | Element el ->
         let lnss = local_namespaces lnss t el in
           out "<";
           out (string_of_qname t el.ns el.name);
           if el.attrs <> [] then (
             out " ";
             out (string_of_list (string_of_attr t) " " el.attrs)
           );
           if lnss <> [] then (
             out " ";
             out (string_of_list (string_of_ns t) " " lnss));
           if el.children = [] then
             out "/>"
           else (
             out ">";
             List.iter (aux_serialize []
                          {t with default_nss = lnss @ t.default_nss} 
                          out) el.children;
             out "</";
             out (string_of_qname t el.ns el.name);
             out ">"
           )
     | Element.Cdata text ->
         out (encode text)
           
   let serialize_document t out xml =
     aux_serialize t.default_nss t out xml
       
end

let get_attr_value ?ns name el =
  let attr =
    List.find (fun attr ->
                 match ns with
                   | None ->
                       no_ns = attr.Attribute.ns && name = attr.Attribute.name
                   | Some v ->
                       v = attr.Attribute.ns && name = attr.Attribute.name
              ) el.Element.attrs
  in
    attr.Attribute.value
      
let safe_get_attr_value ?ns name el =
  try get_attr_value ?ns name el with Not_found -> ""
     
let get_element ns name els =
  List.find (function
               | Element.Element el ->
                   el.Element.ns = ns && el.Element.name = name
               | Element.Cdata _ ->
                   false
            ) els
    
let get_elements ns name childs =
  List.filter (function
                 | Element.Element el -> el.Element.ns = ns && el.Element.name = name
                 | Element.Cdata _ -> false
              ) childs
    
let get_subelement ns name el =
  get_element ns name el.Element.children
    
let get_subelements ns name el =
  get_elements ns name el.Element.children
    
let get_first_subelement el =
  List.find (function
               | Element.Element _ -> true
               | Element.Cdata _ -> false
            ) el.Element.children
    
let collect_cdata  els =
  let res =List.fold_left (fun acc -> function
                             | Element.Cdata cdata -> cdata :: acc
                             | Element.Element _ -> acc
                          ) [] els in
    String.concat "" (List.rev res)

let get_cdata el =
  collect_cdata el.Element.children
      
let remove_cdata els =
  List.filter (function
                 | Element.Element _ -> true
                 | Element.Cdata _ -> false) els
    
let make_element ns name attrs children =
  { Element.ns = ns; Element.name = name; attrs = attrs; children = children}
    
let make_attr ?ns name value =
  let ns = match ns with None -> no_ns | Some v -> v in
    {Attribute.ns = ns; Attribute.name = name; value = value}
    
let make_simple_cdata ns name cdata =
  make_element ns name [] [Element.Cdata cdata]
    
let mem_element ns name els =
  List.exists (function
                 | Element.Element el ->
                     el.Element.ns = ns && el.Element.name = name
                 | Element.Cdata _ -> false
              ) els
    
(*
 * Parsing
 *)

open Element

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
                        let ns = if value = "" then None else Some value in
                          ((ns, "") :: nss), attrs
                      else if prefix = "xmlns" && lname <> "" then
                        let ns = if value = "" then None else Some value in
                          ((ns, lname) :: nss) , attrs
                      else
                        nss, (((prefix, lname), value) :: attrs)
                 ) ([], []) attrs
    
let add_namespaces namespaces nss =
  List.iter (fun (ns, prefix) -> Hashtbl.add namespaces prefix ns) nss
    
let remove_namespaces namespaces nss =
  List.iter (fun (_ns, prefix) -> Hashtbl.remove namespaces prefix) nss
    
let parse_qname nss (prefix, lname) =
  if prefix = "" then
    (no_ns, lname)
  else
    try
      let namespace = Hashtbl.find nss prefix in
        (namespace, lname)
    with Not_found ->
      (Some prefix, lname)
      
let parse_qname_attribute nss (prefix, lname) value = 
  if prefix = "" then
    {Attribute.ns = no_ns; Attribute.name = lname; value = value}
  else
    try
      let ns = Hashtbl.find nss prefix in
        {Attribute.ns = ns; Attribute.name = lname; value = value}
    with Not_found ->
      {Attribute.ns = Some prefix; Attribute.name = lname; value = value}
        
let parse_attrs nss attrs =
  List.map (fun (name, value) -> parse_qname_attribute nss name value) attrs
    
let parse_element_head namespaces name attrs =
  let lnss, attrs = split_attrs attrs in
    add_namespaces namespaces lnss;
    let qname = parse_qname namespaces (split_name name) in
    let attrs = parse_attrs namespaces attrs in
      qname, lnss, attrs
        
let string_of_tag (ns, name) =
  let prefix =
    match ns with
      | None -> ""
      | Some str -> "URI " ^ str
  in
    Printf.sprintf "(%S) %s" prefix name
      
let process_production (state, tag) =
  let namespaces = Hashtbl.create 1 in
  let () = Hashtbl.add namespaces "xml" ns_xml in
    
  let rec process_prolog (state, tag) =
    match tag with
      | Xmlparser.Comment _
      | Xmlparser.Doctype _
      | Xmlparser.Pi _
      | Xmlparser.Whitespace _ ->
          process_prolog (Xmlparser.parse state)
      | Xmlparser.StartElement (name, attrs) ->
          let (ns, name), lnss, attrs =
            parse_element_head namespaces name attrs in
          let nextf el state =
            remove_namespaces namespaces lnss;
            process_epilogue el (Xmlparser.parse state)
          in
            get_childs (make_element ns name attrs []) nextf
              (Xmlparser.parse state)
      | Xmlparser.EndOfBuffer ->
          failwith "End of Buffer"
      | Xmlparser.EndOfData ->
          raise End_of_file
      | Xmlparser.Text _
      | Xmlparser.EndElement _ ->
          failwith "Unexpected tag"
            
  and get_childs el nextf (state, tag) =
    match tag with
      | Xmlparser.Whitespace str ->
          get_childs {el with children = Cdata str :: el.children} nextf
            (Xmlparser.parse state)
      | Xmlparser.Text str ->
          get_childs {el with children = Cdata str :: el.children} nextf
            (Xmlparser.parse state)
      | Xmlparser.StartElement (name, attrs) ->
          let (ns, name), lnss, attrs =
            parse_element_head namespaces name attrs in
          let newnextf el' state =
            remove_namespaces namespaces lnss;
            get_childs {el with children = Element el' :: el.children} nextf
              (Xmlparser.parse state)
          in
            get_childs (make_element ns name attrs []) newnextf
              (Xmlparser.parse state)
      | Xmlparser.EndElement name ->
          let (ns, name) = parse_qname namespaces (split_name name) in
            if ns = el.ns && name = el.name then
              nextf {el with children = List.rev el.children} state
            else 
              failwith (Printf.sprintf "Bad end tag: expected %s, was %s"
                          (string_of_tag (ns, name))
                          (string_of_tag (el.ns, el.name)))
      | Xmlparser.Comment _
      | Xmlparser.Pi _ ->
          get_childs el nextf (Xmlparser.parse state)
      | Xmlparser.Doctype _dtd ->
          failwith "Doctype declaration inside of element"
      | Xmlparser.EndOfBuffer ->
          failwith "End of Buffer"
      | Xmlparser.EndOfData ->
          raise End_of_file
            
  and process_epilogue el (state, tag) =
    match tag with
      | Xmlparser.Comment _
      | Xmlparser.Pi _
      | Xmlparser.Whitespace _ ->
          process_epilogue el (Xmlparser.parse state)
      | Xmlparser.EndOfBuffer ->
          failwith "End Of Buffer"
      | Xmlparser.EndOfData ->
          el
      | Xmlparser.Text _
      | Xmlparser.Doctype _
      | Xmlparser.StartElement _
      | Xmlparser.EndElement _ ->
          failwith "Invalid epilogue"
  in
    process_prolog (state, tag)

let parse_document ?unknown_encoding_handler ?entity_resolver buf =
  let p = Xmlparser.create ?unknown_encoding_handler ?entity_resolver () in
    process_production (Xmlparser.parse ~buf ~finish:true p)
      
