(*
 * (c) 2007-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

exception Malformed

(*
type uchar = int
*)

let of_char = Char.code

let u_lt = Char.code '<'
let u_gt = Char.code '>'
let u_slash = Char.code '/'
let u_excl = Char.code '!'
let u_quest = Char.code '?'
let u_amp = Char.code '&'
let u_dash = Char.code '-'
let u_openbr = Char.code '['
let u_closebr = Char.code ']'
let u_dot = Char.code '.'
let u_colon = Char.code ':'
let u_semicolon = Char.code ';'
let u_underline = Char.code '_'
let u_eq = Char.code '='
let u_quot = Char.code '"'
let u_apos = Char.code '\''
let u_lf = Char.code '\n'
let u_cr = Char.code '\r'
let u_tab = Char.code '\t'
