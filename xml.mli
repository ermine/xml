(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
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
    Xmlelement of qname * attribute list * element list
  | Xmlcdata of cdata

val ns_xml : namespace
val no_ns : namespace

val encode : string -> string
val decode : string -> string

module Serialization :
  sig
    type t = {
      default_nss : namespace list;
      bindings : (string, string) Hashtbl.t;
    }
    val bind_prefix : t -> string -> namespace -> unit
    val create : namespace list -> t
    val string_of_qname : t -> qname -> string
    val string_of_attr : t -> attribute -> string
    val string_of_list : ('a -> string) -> string -> 'a list -> string
    val string_of_ns : t -> namespace -> string
    val local_namespaces :
      t -> qname -> attribute list -> namespace list -> namespace list
    val aux_serialize :
      namespace list -> t -> (string -> unit) -> element -> unit
    val serialize_document : t -> (string -> unit) -> element -> unit
  end

val get_qname : element -> qname
val get_namespace : qname -> namespace
val get_name : qname -> string
val get_attrs : ?ns:namespace -> element -> attribute list
val get_attr_value : ?ns:namespace -> name ->
  attribute list -> cdata
val safe_get_attr_value : ?ns:namespace -> name ->
  attribute list -> string
val get_element : qname -> element list -> element
val get_elements : qname -> element list -> element list
val get_children : element -> element list
val get_subelement : qname -> element -> element
val get_subelements : qname -> element -> element list
val get_first_element : element list -> element
val get_cdata : element -> string
val remove_cdata : element list -> element list
val make_element : qname -> attribute list -> element list -> element
val make_attr: ?ns:namespace  -> name -> cdata -> attribute
val make_simple_cdata : qname -> cdata -> element
val mem_qname : qname -> element list -> bool
val mem_child : qname -> element -> bool
val iter : (element -> unit) -> element -> unit

val split_name : string -> prefix * ncname

val split_attrs :
  (string * string) list ->
  (namespace * string) list * ((string * string) * string) list
val add_namespaces :
  (prefix, namespace) Hashtbl.t -> (namespace * prefix) list -> unit
val remove_namespaces :
  (prefix, namespace) Hashtbl.t -> (namespace * prefix) list -> unit
val parse_qname :
  (prefix, namespace) Hashtbl.t -> prefix * string -> qname
val parse_qname_attribute :
  (prefix, namespace) Hashtbl.t -> prefix * string -> qname
val parse_attrs :
  (prefix, namespace) Hashtbl.t ->
  ((prefix * string) * string) list -> (qname * string) list
val parse_element_head :
  (prefix, namespace) Hashtbl.t ->
  string -> 
  (string * string) list ->
  qname * (namespace * prefix) list * attribute list
val string_of_tag : qname -> string

val parse_document :
  ?unknown_encoding_handler:(string -> (string -> int -> Xmlencoding.t)) ->
  ?entity_resolver:(string -> string) -> string -> element

  
