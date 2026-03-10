/* Parser for Markdowns */

%{
  open Ast
%}


/* Tokens */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT

%token FLOAT BOOL STRING
%token ABBR
%token IF ELSE
%token AND OR NOT

%token EQUAL
%token PLUS MINUS TIMES DIV
%token LP RP
%token LBRACE RBRACE
%token SEMI
%token COMMA COLON

%token OPEN_CODE CLOSE_CODE
%token ANNOTATION
%token <string> TEXT

%token EOF

/* Precedence - lowest to highest */
%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV

/* Entry point */
%start file
%type <Ast.document> file

%%

file:
| blocks = list(block) EOF      { blocks }
;

block:
| OPEN_CODE s = stmts CLOSE_CODE  { Script s }
| ANNOTATION e = expr             { Annotation e }
| t = TEXT                        { RawText t }
;

stmts:
| s = stmt                        { s }
| s = stmt rest = stmts           { Sseq (s, rest) }
;

stmt:
| id = ident EQUAL e = expr       { Sassign (id, e) }
| IF c = expr s1 = stmt ELSE s2 = stmt
    { Sif (c, s1, s2) }
| IF c = expr s1 = stmt
    { Sif (c, s1, Snop) }
;

expr:
| c = CST                         { Ec c }
| id = ident                      { Eident id }
| e1 = expr o = binop e2 = expr   { Ebinop (o, e1, e2) }
| e1 = expr o = CMP  e2 = expr    { Ebinop (o, e1, e2) }
| NOT e1 = expr                   { Eunop (Unot, e1) }
| LP e = expr RP                  { e }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| AND   { Band }
| OR    { Bor  }
;

ident:
| id = IDENT { { loc = ($startpos, $endpos); id } }
;
