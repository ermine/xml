{
}

rule token = parse
   | "$"
	 { DOLLAR }
   | "("
	 { LPAREN }
   | ")"
	 { RPAREN }
   | "["
	 { LBRACKET }
   | "]"
	 { RBRACKET }
   | ","
	 { COMMA }
   | "*"
	 { STAR }
   | "?"
	 { QUEST }
   | "@"
	 { AMP }
   | "in"
	 { IN }
   | "or"
	 { OR }
   | "and"
	 { AND }
   | "to"
	 { TO }


и
[71]    IntegerLiteral    ::=    Digits                                         
[72]    DecimalLiteral    ::=    ("." Digits) | (Digits "." [0-9]*)             
                                                                                
[73]    DoubleLiteral     ::=    (("." Digits) | (Digits ("." [0-9]*)?)) [eE]   
                                 [+-]? Digits                                   
[74]    StringLiteral     ::=    ('"' (EscapeQuot | [^"])* '"') | ("'"          
                                 (EscapeApos | [^'])* "'")                      
[75]    EscapeQuot        ::=    '""'                                           
[76]    EscapeApos        ::=    "''"                                           
[77]    Comment           ::=    "(:" (CommentContents | Comment)* ":)"         
                                                                                
                                                                                
                                                                                
[78]    QName             ::=    [http://www.w3.org/TR/REC-xml-names/#NT-QName] 
                                 ^Names                                         
                                                                                
[79]    NCName            ::=    [http://www.w3.org/TR/REC-xml-names/#NT-NCName]
                                 ^Names                                         
                                                                                
[80]    Char              ::=    [http://www.w3.org/TR/REC-xml#NT-Char] ^XML    
