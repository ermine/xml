exception LexerError of string
exception UnknownEntity of string

type production =
    StartElement of string * (string * string) list
  | EndElement of string
  | EmptyElement of string * (string * string) list
  | Pi of string * string
  | Comment of string
  | Whitespace of string
  | Cdata of string
  | Text of string
  | Doctype of string * Xml.external_id option * string
  | EOD

type cb = production -> ('a -> unit) -> unit as 'a

type parser_t = {
  mutable encoding : string;
  mutable fparser : parser_t -> cb -> unit;
  mutable fencoder : int -> (int, char list) Fstream.t;
  mutable strm : char Stream.t;
  mutable nextf : cb;
  entity_handler : string -> int;
  encoding_handler : string -> char -> (char, int) Fstream.t;
}
and lstream =
    Lexer of (parser_t -> int -> lstream)
  | Switch of (char -> (char, int) Fstream.t) * (parser_t -> int -> lstream)
  | Token of production * (parser_t -> int -> lstream)

val create :
  ?encoding:Encoding.encoding ->
  ?process_unknown_encoding:(string -> char -> (char, int) Fstream.t) ->
  ?process_entity:(string -> int) ->
  ?process_production:cb -> unit -> parser_t

val set_callback : parser_t -> cb -> unit

val parse : parser_t -> string -> int -> int -> unit

val finish : parser_t -> unit

val reset : parser_t -> cb -> unit

val get_rest_buffer : parser_t -> string

val decode : string -> string

val encode : string -> string

val split_name : string -> string * string
