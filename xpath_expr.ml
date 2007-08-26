let compile_xpath_step abc =
   


let parse str =
   let lexbuf = Ulexing.from_utf8_string in
      Xpath_grammar.expr Xpath_ukex,token lexbuf

let compile str =
   let parsed = parse str in
      compile_xpath_expr parsed
