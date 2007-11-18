(*
 * (c) 2007 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

exception NonXmlelement

type element =
    Xmlelement of Xml.qname * Xml.ns_mapping list * Xml.attribute list *
      element list
  | Xmlcdata of Xml.cdata

val string_of_attr : Xml.qname * Xml.cdata -> string

val string_of_ns : Xml.ns_mapping -> string

val serialize : (string -> unit) -> element -> unit

val get_tag : element -> Xml.qname

val get_namespaces : element -> Xml.ns_mapping list

val get_namespace : Xml.qname -> Xml.namespace

val get_ns_mapping : Xml.qname -> Xml.ns_mapping

val get_name : Xml.qname -> Xml.ncname

val get_attrs : element -> Xml.attribute list

val get_attr_value : Xml.qname -> Xml.attribute list -> string

val safe_get_attr_value : Xml.qname -> Xml.attribute list -> string

val get_element : Xml.qname -> element list -> element

val get_elements : Xml.qname -> element list -> element list

val get_subelement : Xml.qname -> element -> element

val get_subelements : Xml.qname -> element -> element list

val get_children : element -> element list

val get_first_element : element list -> element

val get_cdata : element -> string

val remove_cdata : element list -> element list

val make_element :
  Xml.qname ->
  Xml.ns_mapping list -> Xml.attribute list -> element list -> element

val make_simple_cdata : Xml.ncname -> Xml.cdata -> element

val mem_qname : Xml.qname -> element list -> bool

val mem_child : Xml.qname -> element -> bool

val iter : (element -> unit) -> element -> unit

val split_attrs :
  (string * string) list ->
  Xml.ns_mapping list * ((string * string) * string) list

val add_namespaces : 
  (Xml.ncname, Xml.namespace) Hashtbl.t -> Xml.ns_mapping list -> unit

val remove_namespaces :
  (Xml.ncname, Xml.namespace) Hashtbl.t -> Xml.ns_mapping list -> unit

val parse_qname :
  (Xml.ncname, Xml.namespace) Hashtbl.t ->
  Xml.ncname * Xml.ncname -> Xml.qname

val parse_qname_attribute :
  (Xml.ncname, Xml.namespace) Hashtbl.t ->
  Xml.ncname * Xml.ncname -> Xml.qname

val parse_attrs :
  (Xml.ncname, Xml.namespace) Hashtbl.t ->
  ((Xml.ncname * Xml.ncname) * Xml.cdata) list -> Xml.attribute list

val parse_element_head :
  (Xml.ncname, Xml.namespace) Hashtbl.t ->
  string ->
  (string * string) list ->
  Xml.qname * Xml.ns_mapping list * Xml.attribute list

val process_production :
  (element -> 'a) -> (Xmlparser.production -> ('b -> 'a) -> 'a as 'b)

val parse_document : string -> (element -> unit) -> unit
