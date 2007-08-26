(*
 * (c) 2004, 2005, 2006, 2007 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

exception NonXmlelement

type element =
   | Xmlelement of string * (string * string) list * element list
   | Xmlcdata of string

let decode = Xml_decode.decode
let encode = Xml_encode.encode

let rec to_string ?encode =
   let rec attrs_to_string attrs =
      let attr_to_string (name, value) = Printf.sprintf " %s='%s'" name 
	 (if encode = Some true then Xml_encode.encode value else value) in
         String.concat "" (List.map attr_to_string attrs)
   in
      function
         | Xmlelement (name, attrs, []) ->
              Printf.sprintf "<%s%s/>" name (attrs_to_string attrs)
         | Xmlelement (name, attrs, els) ->
                 Printf.sprintf "<%s%s>%s</%s>" 
                    name
                    (attrs_to_string attrs)
                    (String.concat "" (List.map (to_string ?encode) els))
                    name
         | Xmlcdata cdata -> 
	      if encode = Some true then
		 Xml_encode.encode cdata
	      else
		 cdata

let get_tag = function
   | Xmlelement (name, _, _) -> name
   | _ -> raise NonXmlelement

let get_cdata = function
   | Xmlelement (_, _, els) ->
        let rec collect_cdata els acc =
           match els with
              | (Xmlcdata cdata) :: l -> collect_cdata l (cdata :: acc)
              | _ :: l -> collect_cdata l acc
              | [] -> String.concat "" (List.rev acc)
        in
           collect_cdata els []
   | _ -> raise NonXmlelement

let get_attrs = function
   | Xmlelement (_, attrs, _) -> attrs
   | _ -> raise NonXmlelement

let get_attr_s name = function
   | Xmlelement (_, attrs, _) ->
	List.assoc name attrs
   | _ -> raise NonXmlelement

let safe_get_attr_s name xml =
   try get_attr_s name xml with Not_found -> ""

let get_children = function
   | Xmlelement (_, _, els) -> els
   | _ -> raise NonXmlelement

let get_first =
   List.find (function
      | Xmlelement _ -> true
      | _ -> false)

let get_subelements = function
   | Xmlelement (_, _, subels) ->
	List.filter (function
	   | Xmlelement _ -> true
	   | _ -> false) subels
   | _ -> raise NonXmlelement

let iter f xml =
   List.iter f (get_children xml)

let map f xml =
   List.map f (get_children xml)

let make_simple_cdata name cdata =
   Xmlelement (name, [], [Xmlcdata cdata])

let make_element name (attrs:(string * string) list) (subels:element list) =
   Xmlelement (name, attrs, subels)
