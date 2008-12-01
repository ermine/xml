(*
 * (c) 2007-2008, Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

exception LexerError of string
exception UnknownEntity of string

type data =
   | UCS4 of int
   | EOB
   | EOD

type name = string

type external_id = string * string

type quantify = [
| `One
| `Plus
| `Quest
| `Star
]

type contentspec_children = [
| `Choice of cp list * quantify
| `Seq of cp list * quantify
]
and cp = [
| `Name of name * quantify
| `Choice of cp list * quantify
| `Seq of cp list * quantify
]

type contentspec = [
| `EMPTY
| `ANY
| `Mixed of name list
| `Children of contentspec_children
]

type atttype = [
| `CDATA
| `ID
| `IDREF
| `IDREFS
| `ENTITY
| `ENTITIES
| `NMTOKEN
| `NMTOKENS
| `NOTATION of name list
| `ENUMERATION of string list
]

type defaultdecl = [
| `REQUIRED
| `IMPLIED
| `FIXED of string
| `Default of string
]

type parameter_entity_value = [
| `EntityValue of string
| `ExternalID of external_id
]

type entity_value = [
| `EntityValue of string
| `ExternalID of external_id
| `UnparsedExternalID of external_id * name
]

type entitydecl = [
| `ParameterEntity of name * parameter_entity_value
| `Entity of name * entity_value
]

type intsub = [ 
| `Elementdecl of name * contentspec
| `DeclSect of name
| `AttlistDecl of name * (name * atttype * defaultdecl) list
| `EntityDecl of entitydecl
| `NotationDecl of name * external_id
| `PI of string * string
| `Comment of string
]

type dtd = {
   dtd_name : name;
   dtd_external_id : external_id option;
   dtd_intsubset : intsub list;
}

type production =
    StartElement of name * (name * string) list
  | EndElement of name
  | EmptyElement of name * (name * string) list
  | Pi of name * string
  | Comment of string
  | Whitespace of string
  | Cdata of string
  | Text of string
  | Doctype of dtd
  | EndOfBuffer
  | EndOfData

type parser_t = {
  is_parsing : bool;
  finish: bool;
  strm : char Stream.t;
  fparser : parser_t -> char Stream.t -> (production * parser_t);
  encoding : string;
  mutable fencoder : int -> (int, char list) Fstream.t;
  entity_resolver : string -> string;
  encoding_handler : string -> (char -> (char, int) Fstream.t);
}
and lstream =
    Lexer of (parser_t -> data -> lstream)
  | Switch of (char -> (char, int) Fstream.t) * (parser_t -> data -> lstream)
  | Token of production * (parser_t -> data -> lstream)

val string_of_production : production -> string

val create :
  ?encoding:Xmlencoding.encoding ->
  ?unknown_encoding_handler:(string -> char -> (char, int) Fstream.t) ->
  ?entity_resolver:(string -> string) -> unit -> parser_t

val parse : ?buf:string -> ?finish:bool -> parser_t -> (production * parser_t)

val parse_dtd : string -> production * parser_t

val set_entity_resolver : parser_t -> (string -> string) -> parser_t

val reset : parser_t -> parser_t

val get_rest_buffer : parser_t -> string

val decode : string -> string

val encode : string -> string

val split_name : string -> string * string
