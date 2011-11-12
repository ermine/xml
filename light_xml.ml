(*
 * (c) 2004-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

type element =
  | Xmlelement of string * (string * string) list * element list
  | Xmlcdata of string

exception NonXmlelement
exception Expected of string

let decode = Xml_decode.decode
let encode = Xml_encode.encode

let rec attrs_to_string attrs =
  let attr_to_string attr =
    match attr with
      | (name, value) -> 
	        Printf.sprintf " %s='%s'" name (encode value)
  in List.fold_left (^) "" (List.map attr_to_string attrs)
       
let rec element_to_string el =
  match el with
    | Xmlelement (name, attrs, els) ->
        if List.length els > 0 then
          (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ ">" ^
            (List.fold_left (^) "" (List.map element_to_string els)) ^
            (Printf.sprintf "</%s>" name)
        else
          (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ "/>"
    | Xmlcdata chunk -> encode chunk
        
let rec get_tag (el:element) (path:string list) =
  match el with
    | Xmlelement (_,_, els) ->
        if path = [] then el
        else
          let name = List.hd path in
          let ctag = List.find
            (function
               | Xmlelement (name1, _,_) ->
                   name = name1
               | Xmlcdata _ ->
			             false
            ) els in
            get_tag ctag (List.tl path)
    | Xmlcdata _ -> raise NonXmlelement
        
let get_tag_full_path el path =
  match el with
    | Xmlelement (tag, _,_) ->
        if tag = List.hd path then get_tag el (List.tl path)
        else raise Not_found
    | Xmlcdata _cdata -> 
	      raise NonXmlelement
          
let get_subel ?(path=[]) el =
  match get_tag el path with
    | Xmlelement (_, _, els) ->
	      List.find (function
			               | Xmlelement (_, _, _) -> true
			               | Xmlcdata _ -> false
		              ) els
    | Xmlcdata _ -> raise NonXmlelement
        
let get_subels ?(path=[]) ?(tag="") el =
  match get_tag el path with
    | Xmlelement (_, _, els) ->
        if tag = "" then els
	      else if els = [] then []
	      else
          List.find_all (function x ->
                           match x with
                             | Xmlelement (tag1, _,_) -> tag1 = tag
                             | Xmlcdata _ -> false
                        ) els
    | Xmlcdata _ -> 
	      raise NonXmlelement
          
let get_attr_s el ?(path=[]) (attrname:string) =
  match get_tag el path with
    | Xmlelement (_, attrs, _) ->
        List.assoc attrname attrs
    | Xmlcdata _ -> raise NonXmlelement
        
let filter_attrs attrs =
  let checker (_k,v) = if v = "" then false else true in
    List.filter checker attrs
      
let rec collect_cdata els acc =
  match els with
    | [] -> String.concat "" (List.rev acc)
    | (Xmlcdata cdata) :: l -> collect_cdata l (cdata :: acc)
    | Xmlelement _ :: l -> collect_cdata l acc
        
let get_cdata ?(path=[]) el =
  match get_tag el path with
    | Xmlelement (_, _, els) -> collect_cdata els []
    | Xmlcdata _ -> raise NonXmlelement
        
let make_element name attrs els =
  Xmlelement (name, attrs, els)
    
let make_simple_cdata name cdata =
  Xmlelement (name, [], [Xmlcdata cdata])
    
let safe_get_attr_s xml ?(path=[]) attrname =
  try get_attr_s xml ~path attrname with _ -> ""
    
let match_tag tag element =
  let b = 
    match element with
      | Xmlelement (tag1, _, _) -> tag1 = tag
      | Xmlcdata _ -> false
  in
    if not b then
	    raise (Expected tag)
          
let exists_element tag els =
  List.exists (function
		             | Xmlelement (tag1, _, _) -> tag1 = tag
		             | Xmlcdata _ -> false
	            ) els
    
    
let find_subtag (subels:element list) (tag:string) =
  List.find (function
               | Xmlelement (tag1, _, _) -> tag1=tag
               | Xmlcdata _ -> false
            ) subels
    
let get_tagname el =
  match el with
    | Xmlelement (name, _, _) -> name
    | Xmlcdata _ -> raise NonXmlelement
        
let match_xml el tag (attrs:(string * string) list) =
  match el with
    | Xmlelement (name, _, _) ->
        if name = tag then
	        (try
             List.iter (fun (a, v) ->
			                    if get_attr_s el a <> v then 
				                    raise Not_found) attrs;
             true
           with _ -> false)
        else
          false
    | Xmlcdata _ -> false
        
let mem_xml xml path tag attrs =
  if get_tagname xml <> List.hd path then false
  else
    try
	    let els = get_subels xml ~path:(List.tl path) ~tag in
	      List.exists (fun el ->
			                 try
			                   List.iter (fun (a, v) -> 
					                            if get_attr_s el a <> v 
					                            then raise Not_found) attrs;
			                   true
			                 with _ -> false
			              ) els
    with _ -> false
      
let get_by_xmlns xml ?path ?tag xmlns =
  let els = get_subels xml ?path ?tag in
    List.find (fun x ->
		             if safe_get_attr_s x "xmlns" = xmlns then true
		             else false) els
      
let process_production (state, tag) =
  let rec process_prolog (state, tag) =
    match tag with
      | Xmlparser.Comment _
      | Xmlparser.Doctype _
      | Xmlparser.Pi _
      | Xmlparser.Whitespace _ ->
          process_prolog (Xmlparser.parse state)
      | Xmlparser.StartElement (name, attrs) ->
          let nextf childs (state, tag) =
            let el = Xmlelement (name, attrs, childs) in
              process_epilogue el (state, tag)
          in
            get_childs name nextf [] (Xmlparser.parse state)
      | Xmlparser.EndOfBuffer ->
          failwith "End of Buffer"
      | Xmlparser.EndOfData ->
          raise End_of_file
      | Xmlparser.EndElement _
      | Xmlparser.Text _ ->
          failwith "Unexpected tag"
            
  and get_childs name nextf childs (state, tag) =
    match tag with
      | Xmlparser.Whitespace str ->
          get_childs name nextf (Xmlcdata str :: childs) (Xmlparser.parse state)
      | Xmlparser.Text str ->
          get_childs name nextf (Xmlcdata str :: childs) (Xmlparser.parse state)
      | Xmlparser.StartElement (name', attrs) ->
          let newnextf childs' (state, tag) =
            let child = 
              Xmlelement (name', attrs, childs') in
              get_childs name nextf (child :: childs) (state, tag)
          in
            get_childs name' newnextf [] (Xmlparser.parse state)
      | Xmlparser.EndElement name' ->
          if name = name' then
            nextf (List.rev childs) (Xmlparser.parse state)
          else 
            failwith (Printf.sprintf "Bad end tag: expected %s, was %s"
                        name name')
      | Xmlparser.Comment _
      | Xmlparser.Pi _ ->
          get_childs name nextf childs (Xmlparser.parse state)
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
