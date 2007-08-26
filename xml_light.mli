(*
 * (c) 2004, 2005, 2006, 2007 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

exception NonXmlelement

type element =
    Xmlelement of string * (string * string) list * element list
  | Xmlcdata of string

val decode : string -> string

val encode : string -> string

val to_string : ?encode:bool -> element -> string

val get_tag : element -> string

val get_cdata : element -> string

val get_attrs : element -> (string * string) list

val get_attr_s : string -> element -> string

val safe_get_attr_s : string -> element -> string

val get_children : element -> element list

val get_first : element list -> element

val get_subelements : element -> element list

val iter : (element -> unit) -> element -> unit

val map : (element -> 'a) -> element -> 'a list

val make_simple_cdata : string -> string -> element

val make_element :
  string -> (string * string) list -> element list -> element
