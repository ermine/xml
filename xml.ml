(*
 * (c) 2007-2012 Anastasia Gornostaeva
 * 
 * http://www.w3.org/TR/xml (fourth edition)
 * http://www.w3.org/TR/REC-xml-names
 *)

exception NonXmlelement
exception InvalidNS

type namespace = string option

type prefix = string

type ncname = string

type name = ncname

type qname = namespace * name

type cdata = string

type attribute = qname * cdata

type element = 
  | Xmlelement of (qname * attribute list * element list)
  | Xmlcdata of cdata

let ns_xml = Some "http://www.w3.org/XML/1998/namespace"

let no_ns = None

let encode = Xml_encode.encode
let decode = Xml_decode.decode

module Serialization =
struct
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

   let string_of_qname t (ns, name) =
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
                 
   let string_of_attr t (qname, value) =
     (string_of_qname t qname) ^ "='" ^ encode value ^ "'"
       
   let string_of_list f sep = function
     | [] -> ""
     | x :: [] -> f x
     | x :: xs -> List.fold_left (fun res x -> res ^ sep ^ (f x)) (f x) xs
         
   let local_namespaces lnss t (ns, _name) attrs =
     let lnss =
       if List.mem ns lnss || List.mem ns t.default_nss then
         lnss
       else
         ns :: lnss
     in
       List.fold_left (fun acc ((ns, _name), _value) ->
                         if ns = no_ns ||
                           ns = ns_xml ||
                           List.mem ns t.default_nss || 
                           List.mem ns lnss then
                             acc
                         else
                           match ns with
                             | None -> acc
                             | Some str ->
                                 if not (Hashtbl.mem t.bindings str) then (
                                   t.tmp_prefix <- t.tmp_prefix + 1;
                                   let p = "ns" ^ string_of_int t.tmp_prefix in
                                     bind_prefix t p ns;
                                 );
                                 ns :: acc
                      ) lnss attrs
         
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
     | Xmlelement (qname, attrs, children) ->
         let lnss = local_namespaces lnss t qname attrs in
           out "<";
           out (string_of_qname t qname);
           if attrs <> [] then (
             out " ";
             out (string_of_list (string_of_attr t) " " attrs)
           );
           if lnss <> [] then (
             out " ";
             out (string_of_list (string_of_ns t) " " lnss));
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

let get_qname = function
  | Xmlelement (qname, _, _) -> qname
  | Xmlcdata _ -> raise NonXmlelement
      
let get_namespace (namespace, _name) = namespace

let get_name (_namespace, name) = name

let get_attrs ?ns = function
  | Xmlelement (_', attrs, _) -> (
      match ns with
        | None -> attrs
        | Some v -> List.find_all (fun ((ns', _), _) -> ns' = v) attrs
    )
  | Xmlcdata _ -> raise NonXmlelement
      
let get_attr_value ?ns name attrs =
  let (_, value) =
    List.find (fun (qname, _) ->
                 match ns with
                   | None -> (no_ns, name) = qname
                   | Some v -> (v, name) = qname
              ) attrs
  in
    value
      
let safe_get_attr_value ?ns name attrs =
  try get_attr_value ?ns name attrs with Not_found -> ""
     
let get_element qname childs =
  List.find (function
               | Xmlelement (qname', _, _) -> qname = qname'
               | Xmlcdata _ -> false
            ) childs
    
let get_elements qname childs =
  List.filter (function
                 | Xmlelement (qname', _, _) -> qname = qname'
                 | Xmlcdata _ -> false
              ) childs
    
let get_children = function
  | Xmlelement (_, _, children) -> children
  | Xmlcdata _ -> raise NonXmlelement
      
let get_subelement qname el =
  get_element qname (get_children el)
    
let get_subelements qname el =
  get_elements qname (get_children el)
    
let get_first_element els =
  List.find (function
               | Xmlelement _ -> true
               | Xmlcdata _ -> false) els
    
let collect_cdata  els =
  let res =List.fold_left (fun acc -> function
                             | Xmlcdata cdata -> cdata :: acc
                             | Xmlelement _ -> acc
                          ) [] els in
    String.concat "" (List.rev res)

let get_cdata el =
  collect_cdata (get_children el)
      
let remove_cdata els =
  List.filter (function
                 | Xmlelement _ -> true
                 | Xmlcdata _ -> false) els
    
let make_element qname attrs children =
  Xmlelement (qname, attrs, children)
    
let make_attr ?ns name value =
  let ns = match ns with None -> no_ns | Some v -> v in
    (ns, name), value
    
let make_simple_cdata qname cdata =
  Xmlelement (qname, [], [Xmlcdata cdata])
    
let mem_qname qname els =
  List.exists (function
                 | Xmlelement (qname', _, _) -> qname = qname'
                 | Xmlcdata _ -> false) els
    
let mem_child qname el =
  mem_qname qname (get_children el)
      
let iter f el = List.iter f (get_children el)
  
(*
 * Parsing
 *)

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
  try
    let namespace = Hashtbl.find nss prefix in
      (namespace, lname)
  with Not_found ->
    (no_ns, lname)
      
let parse_qname_attribute nss (prefix, lname) = 
  if prefix = "" then
    (no_ns, lname)
  else
    try
      let ns = Hashtbl.find nss prefix in
        (ns, lname)
    with Not_found ->
      (no_ns, lname)
        
let parse_attrs nss attrs =
  List.map (fun (name, value) -> parse_qname_attribute nss name, value) attrs
    
let parse_element_head namespaces name attrs =
  let lnss, attrs = split_attrs attrs in
    add_namespaces namespaces lnss;
    let qname = parse_qname namespaces (split_name name) in
    let attrs =  parse_attrs namespaces attrs in
      qname, lnss, attrs
        
let string_of_tag (ns, name) =
  let prefix =
    match ns with
      | None -> ""
      | Some str -> "URI " ^ str
  in
    Printf.sprintf "(%S) %s" prefix name
      

module XmlParser = Xmllexer.M
module XStanza = Xmllexer.XmlStanza
open XStanza
      
let parse_document strm =
  let next_token = XmlParser.make_lexer strm in
  let namespaces = Hashtbl.create 1 in
  let () = Hashtbl.add namespaces "xml" ns_xml in
  let stack = Stack.create () in
  let add_element el =
    let (qname, attrs, subels) = Stack.pop stack in
      Stack.push (qname, attrs, (el :: subels)) stack
  in
  let rec loop () =
    match next_token () with
      | Some t -> (
        match t with
          | StartTag (name, attrs, selfclosing) ->
            let qname, lnss, attrs = parse_element_head namespaces name attrs in
            let el = (qname, attrs, []) in
              if selfclosing then (
                remove_namespaces namespaces lnss;
                if Stack.is_empty stack then (
                  Stack.push el stack;
                  loop ();
                ) else (
                  add_element (Xmlelement el);
                  remove_namespaces namespaces lnss;
                  loop ()
                )
              ) else (
                Stack.push el stack;
                loop ();
                remove_namespaces namespaces lnss;
                loop ()
              )
          | EndTag _name ->
            (* let qname = parse_qname namespaces (split_name name) in *)
            if Stack.length stack > 1 then 
              add_element (Xmlelement (Stack.pop stack))
            else
              ()
          | Text text ->
            add_element (Xmlcdata text);
            loop ()
          | Doctype _              
          | PI _ ->
            loop ()
      )
      | None -> ()
  in
    try
      loop ();
      let el = Stack.pop stack in
        Xmlelement el
    with XmlParser.Located_exn ((line, col), exn) ->
      match exn with
        | XmlParser.Error msg ->
          Printf.eprintf "%d:%d %s\n" line col msg;
          Pervasives.exit 127
        | XmlParser.Error_ExpectedChar chs ->
          Printf.eprintf "%d:%d Expected '%s'\n" line col
            (String.make 1 (List.hd chs));
          Pervasives.exit 127          
        | XmlParser.Error_CharToken u ->
          let chs = XmlParser.S.encode_unicode u in
          let str = String.create (List.length chs) in
          let rec iteri i = function
            | [] -> ()
            | x :: xs -> str.[i] <- x; iteri (succ i) xs
          in
            iteri 0 chs;
            Printf.eprintf "%d:%d Unexpected character token %S\n" line col str;
            Pervasives.exit 127
        | exn ->
          Printf.eprintf "%d:%d %s\n" line col (Printexc.to_string exn);
          Pervasives.exit 127

