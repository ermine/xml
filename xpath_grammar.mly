%{
   open Xpath
%}

%token <string> StringLiteral
%token <int> IntegerLiteral
%token <float> DecimalLiteral DoubleLiteral
%token <string * string> QName
%token IN AS
%token PLUS MINUS COMMA COLON DCOLON DOT DOTDOT DOLLAR
%token SLASH DSHASH
%token LPAREN RPAREN LBRACKET RBRACKET

%start xpath
%type <Xpath.primary_type> xpath

%%
xpath:
| PathExpr                      { $1 }

PathExpr:
| SLASH                                     { Root }
| SLASH RelativePathExpr                    { slash Root $2 }
| DSLASH RelativePathExpr                   { dslash Root $2 }
| RelativePathExpr                          { $1 }

RelativePathExpr:
| StepExpr                                  { $1 }
| RelativePathExpr SLASH StepExpr           { slash $1 $3 }
| RelativePathExpr DSLASH StepExpr          { dslash $1 $3 }

StepExpr:
| FilterExpr                                 {
/*
| AxisExpr
*/

FilterExpr:
| PrimaryExpr PredicateList                    

PredicateList:
| Predicate*                                   
 
Predicate:
| "[" Expr "]"                                     




/*
xpath:
| Expr                          { $1 }

Expr:
| ExprSingle                     { [$1] }
| ExprSingle COMMA Expr          { $1 :: %2 }

ExprSingle:
| ForExpr                        { ForExpr $1 }
| QuantifiedExpr                 { QantifiedExpr $1 }
| IfExpr                         { IfExpr $1 }
| OrExpr                         { OrExpr $1 }

ForExpr:
| SimpleForClause "return" ExprSingle { Return ($1, $3) }

SimpleForClause:
| "for" VarNameInExprSingle      { For $2 }

VarNameInExprSingle:
| DOLLAR VarName "in" ExprSingle    { [VarNameInExpr( $2, $4)] }
| DOLLAR VarName "in" ExprSingle COMMA VarNameInExprSingle
                                   { VarNameInExpr( $2, $4) :: $6 }

QuantifiedExpr:
| "some" VarNameInExprSingle "satisfies" ExprSingle 
                             {QuantifiedExpr (`Some, $2, $4) }
| "every" VarNameInExprSingle "satisfies" ExprSingle
                             {QuantifiedExpr (`Every, $2, $4) }

IfExpr:
| "if" LPAREN Expr RPAREN "then" ExprSingle "else" ExprSingle
                            { IfExpr ($3, $6, $8) }

OrExpr:
| AndExpr                   { OrExpr $1 }
| AndExpr OR OrExpr         { OrExpr $1

AndExpr:
| ComparisonExpr
| ComparisonExpr AND AndExpr

ComparisonExpr:
| RangeExpr
| RangeExpr Comp RangeExpr

Comp:
| ValueComp 
| GeneralComp 
| NodeComp

RangeExpr:
| AdditiveExpr
| AdditiveExpr TO AdditiveExpr

AdditiveExpr:
| MultiplicativeExpr ((PLUS | MINUS) MultiplicativeExpr )*

MultiplicativeExpr:
| UnionExpr
| UnionExpr ( (STAR | "div" | "idiv" | "mod") UnionExpr )*

UnionExpr:
| IntersectExceptExpr
| IntersectExceptExpr (("union" | "|") IntersectExceptExpr )*

IntersectExceptExpr:
| InstanceofExpr
| InstanceofExpr (("intersect" | "except") InstanceofExpr )*

InstanceofExpr:
| TreatExpr
| TreatExpr "instance" "of" SequenceType 

TreatExpr:
| CastableExpr
| CastableExpr "treat" AS SequenceType

CastableExpr:
| CastExpr
| CastExpr "castable" AS SingleType

CastExpr:
| UnaryExpr
| UnaryExpr "cast" AS SingleType

UnaryExpr:
| (MINUS | PLUS)* ValueExpr

ValueExpr:
| PathExpr

GeneralComp:
| "=" | "!=" | "<" | "<=" | ">" | ">="

ValueComp:
| "eq" 
| "ne" 
| "lt" 
| "le" 
| "gt" 
| "ge"

NodeComp:
| "is" 
| "<<" 
| ">>"

PathExpr:
| SLASH                              { Root }
| SLASH StepExpr PathExprList        { Slash $2 :: $3 }
| DSLASH StepExpr PathExprList       { Dslash $2 :: $3 }
| StepExpr PathExprList              { NoSlash $1 :: $2 }

PathExprList:
|                                    { [] }
| SLASH StepExpr PathExprList        { Slash $2 :: $3 }
| DSLASH StepExpr PathExprList       { DSlash $2 :: $3 }

StepExpr:
| FilterExpr                         { FilterExpr $1 }
| AxisStep                           { AxisStep $1 }

AxisStep:
| ReverseStep PredicateList          {}
| ForwardStep PredicateList          {}

ForwardStep:
| ForwardAxis NodeTest
| AbbrevForwardStep

ForwardAxis:
| "child" DCOLON
| "descendant" DCOLON
| "attribute" DCOLON
| "self" DCOLON
| "descendant-or-self" DCOLON
| "following-sibling" DCOLON
| "following" DCOLON
| "namespace" DCOLON

AbbrevForwardStep:
| NodeTest
| AMP NodeTest

ReverseStep:
| ReverseAxis NodeTest
| AbbrevReverseStep

ReverseAxis:
| "parent" DCOLON
| "ancestor" DCOLON
| "preceding-sibling" DCOLON
| "preceding" DCOLON
| "ancestor-or-self" DCOLON

AbbrevReverseStep:
| DOTDOT

NodeTest:
| KindTest 
| NameTest

NameTest:
| QName
| Wildcard

Wildcard:
| STAR
| NCName COLON STAR
| STAR COLON NCName

FilterExpr:
| PrimaryExpr PredicateList         { FilterExpr ($1, $2) }

PredicateList:
|                                   { [] }
| Predicate PredicateList           { $1 :: $2 }

Predicate:
| LBRACKET Expr RBRACKET            { Predicate $2 }

PrimaryExpr:
| Literal                           { $1 }
| VarRef                            { VarRef $1 }
| ParenthesizedExpr
| ContextItemExpr
| FunctionCall

Literal:
| NumericLiteral                    { $1 }
| StringLiteral                     { String $1 }

NumericLiteral:
| IntegerLiteral                    { Integer $1 }
| DecimalLiteral                    { Decimal $1 }
| DoubleLiteral                     { Double $1 }

VarRef:
| DOLLAR VarName                    { $2 }

VarName:
| QName                             { $1 }

ParenthesizedExpr:
| LPAREN Expr RPAREN                { Expr (Some $2) }
| LPAREN RPAREN                     { Expr None }

ContextItemExpr:
| DOT                               { Dot }

FunctionCall:
| QName LPAREN  RPAREN  
| QName LPAREN ExprSingle (COMMA ExprSingle)* RPAREN  

SingleType:
| AtomicType
| AtomicType QUEST

SequenceType:
| "empty-sequence" LPAREN RPAREN
| ItemType
| ItemType OccurrenceIndicator

OccurrenceIndicator:
| QUEST 
| STAR 
| PLUS

ItemType:
| KindTest                        { $1 }
| "item" LPAREN RPAREN
| AtomicType

AtomicType:
| QName

KindTest:
| DocumentTest                    { DocumentTest $1 }
| ElementTest                     { ElementTest $1 }
| AttributeTest                   { AttributeTest $1 }
| SchemaElementTest               { SchemaElementTest $1 }
| SchemaAttributeTest             { SchemaAttributeTest $1 }
| PITest                          { PiTest $1 }
| CommentTest                     { CommentTest $1 }
| TextTest                        { TextTest }
| AnyKindTest                     { AnyKindTest $1 }

AnyKindTest:
| "node" LPAREN RPAREN

DocumentTest:
| "document-node" LPAREN  RPAREN
| "document-node" LPAREN ElementTest RPAREN
| "document-node" LPAREN SchemaElementTest RPAREN

TextTest:
| "text" LPAREN RPAREN

CommentTest:
| "comment" LPAREN RPAREN

PITest:
| "processing-instruction" LPAREN RPAREN
| "processing-instruction" LPAREN NCName | StringLiteral RPAREN

AttributeTest:
| "attribute" LPAREN (AttribNameOrWildcard (COMMA TypeName)?)? RPAREN

AttribNameOrWildcard:
| AttributeName                       { AttributeName $1 }
| STAR                                { Star }

SchemaAttributeTest:
| "schema-attribute" LPAREN AttributeDeclaration RPAREN

AttributeDeclaration:
| AttributeName                       { AttributeName $1 }

ElementTest:
| "element" LPAREN (ElementNameOrWildcard (COMMA TypeName QUEST?)?)? RPAREN

ElementNameOrWildcard:
| ElementName                         { ElementName $1 }
| STAR                                { Star }

SchemaElementTest:
| "schema-element" LPAREN ElementDeclaration RPAREN

ElementDeclaration:
| ElementName                  { $1 }

AttributeName:
и| QName                       { $1 }

ElementName:
| QName                       { $1 }

TypeName:
| QName                       { $1 }

*/
