open Xpath_grammar

let enclosed lexbuf offset_begin offset_end =
   let len = Ulexing.lexeme_length lexbuf in
      Ulexing.utf8_sub_lexeme lexbuf offset_begin
         (len - (offset_end + offset_begin))

let regexp digit = ['0'-'9']

let regexp ncfirst = xml_letter | '_'

let regexp ncchar = xml_letter | xml_digit
   | '.' | '-' | '_' | xml_combining_char | xml_extender

let rec token = lexer
   | ncfirst ncchar* (':' ncfirst ncchar* )? ->
	let v = Ulexing.utf8_lexeme lexbuf in
	   if String.contains v ':' then
	      let idx = String.index v ':' in
	      let prefix = String.sub v 0 idx in
	      let localname = String.sub v (idx+1) (String.length v - (idx+1)) in
		 QName (prefix, localname)
	   else
	      QName ("", v)
   | digit+ ->
	let v = Ulexing.utf8_lexeme lexbuf in
	   IntegerLiteral (int_of_string v)

   | "." digit+
   | digit "." digit* ->
	let v = Ulexing.utf8_lexeme lexbuf in
	    DecimalLiteral (float_of_string v)
   | (("." digit+) | (digit+ ("." digit* )?)) ["eE"] ["+-"]? digit+ ->
	let v = Ulexing.utf8_lexeme lexbuf in
	 DoubleLiteral (float_of_string v)
   | '"' ( "\"\"" | [^'"'])* '"'
   | "'" ( "''" | [^"'"])* "'" ->
	StringLiteral (enclosed lexbuf 1 1)

   | "in" ->
	IN
   | "as" ->
	AS
   | "+" ->
	PLUS
   | "-" ->
	MINUS
   | "," ->
	COMMA
   | ":" ->
	COLON
   | "$" ->
	DOLLAR
   | "." ->
	DOT
   | ".." ->
	DOTDOT
   | "[" ->
	LBRACKET
   | "]" ->
	RBRACKET
   | "(" ->
	LPAREN
   | ")" ->
	RPAREN
   | "/" ->
	SLASH
   | "//" ->
	DOUBLE_SLASH
