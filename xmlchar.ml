'0'(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

let of_char = Char.code

let u_lt = of_char '<'
let u_gt = of_char '>'
let u_slash = of_char '/'
let u_excl = of_char '!'
let u_quest = of_char '?'
let u_amp = of_char '&'
let u_dash = of_char '-'
let u_openbr = of_char '['
let u_closebr = of_char ']'
let u_dot = of_char '.'
let u_colon = of_char ':'
let u_semicolon = of_char ';'
let u_underline = of_char '_'
let u_eq = of_char '='
let u_quot = of_char '"'
let u_apos = of_char '\''
let u_lf = of_char '\n'
let u_cr = of_char '\r'
let u_tab = of_char '\t'
let u_percent = of_char '%'
let u_openparen = of_char '('
let u_closeparen = of_char ')'
let u_sharp = of_char '#'
let u_pipe = of_char '|'
let u_star = of_char '*'
let u_0 = of_char '0'
let u_9 = of_char '9'
let u_plus = of_char '+'
let u_comma = of_char ','
let u_a = of_char 'a'
let u_z = of_char 'z'
let u_A = of_char 'A'
let u_Z = of_char 'Z'

  
(*
 * [2] Char    ::=    #x9 | #xA | #xD |  /* any Unicode character,
 *                    [#x20-#xD7FF] |    excluding the surrogate blocks,
 *                    [#xE000-#xFFFD] |  FFFE, and FFFF. */
 *                    [#x10000-#x10FFFF]
 *)
let is_xmlchar = function
  | 0x9 
  | 0xA
  | 0xD  -> true
  | u when u >= 0x20 && u <= 0xD7FF -> true
  | u when u >= 0xE000 && u <= 0xFFFD -> true
  | u when u >= 0x10000 && u <= 0x10FFFF -> true
  | _ -> false

(*
 * [4]     NameStartChar    ::=    ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] |    
 *                                  [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D]   
 *                                  | [#x37F-#x1FFF] | [#x200C-#x200D] |         
 *                                  [#x2070-#x218F] | [#x2C00-#x2FEF] |          
 *                                  [#x3001-#xD7FF] | [#xF900-#xFDCF] |          
 *                                  [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]          
 *)
let is_first_namechar uchar =
  (uchar >= u_a && uchar <= u_z) ||
  uchar = u_colon || (uchar >= u_A && uchar <= u_Z) ||
  uchar = u_underline || 
  (uchar >= 0xC0 && uchar <= 0xD6) ||
  (uchar >= 0xD8 && uchar <= 0xF6) ||
  (uchar >= 0xF8 && uchar <= 0x2FF) ||
  (uchar >= 0x370 && uchar <= 0x37D) ||  
  (uchar >= 0x37F && uchar <= 0x1FFF) ||
  (uchar >= 0x200C && uchar <= 0x200D) ||
  (uchar >= 0x2070 && uchar <= 0x218F) ||
  (uchar >= 0x2C00 && uchar <= 0x2FEF) ||
  (uchar >= 0x3001 && uchar <= 0xD7FF) ||
  (uchar >= 0xF900 && uchar <= 0xFDCF) ||
  (uchar >= 0xFDF0 && uchar <= 0xFFFD) ||
  (uchar >= 0x10000 && uchar <= 0xEFFFF)

(*      
 * [4a]    NameChar         ::=    NameStartChar | "-" | "." | [0-9] | #xB7 |
 *                                  [#x0300-#x036F] | [#x203F-#x2040]
 *)
let is_namechar uchar =
  is_first_namechar uchar || uchar = u_dash  || uchar = u_dot ||
  uchar >= u_0 && uchar <= u_9 || uchar = 0xB7 ||
  (uchar >= 0x0300 && uchar <= 0x036F) ||
  (uchar >= 0x203F && uchar <= 0x2040)
  
(*
 * [3] S         ::= (#x20 | #x9 | #xD | #xA)+
 *)
let is_space = function
  | 0x20 | 0x09 | 0x0D | 0x0A -> true
  | _ -> false
      
(*
 * [13] PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9]
 *                        | | [-'()+,./:=?;!*#@$_%]
 *)
let is_pubid_char ucs4 =
  if ucs4 < 0xFF then
    match Char.chr ucs4 with
      | ' ' | '\n' | '\r'
      | 'a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '-' 
      | '\'' | '(' | ')' | '+' 
      | ',' | '.' | '/' | ':' | '=' 
      | '?' | ';' | '!' | '*' | '#' 
      | '@' | '$' | '_' | '%' -> true
      | _ -> false
  else
    false
      
