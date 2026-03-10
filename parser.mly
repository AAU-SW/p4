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
%type <Ast.stmt> file

%%

file:
| blocks = list(block) EOF      { Sblock blocks }
;

block:
| OPEN_CODE s = stmts CLOSE_CODE  { s }
| ANNOTATION e = expr             { Seval e }
| t = TEXT                        { Stext t }
;

stmts:
| s = stmt                        { s }
| s = stmt rest = stmts           { Sseq (s, rest) }
;

stmt:
| FLOAT_T id = ident EQUAL e = expr SEMI    { Sassign (id, e) }
| BOOL_T  id = ident EQUAL e = expr SEMI    { Sassign (id, e) }
| STRING_T id = ident EQUAL e = expr SEMI   { Sassign (id, e) }
| ABBR id = ident EQUAL s = CST SEMI        { Sassign (id, Ec s) }
| IF LP c = expr RP LBRACE s1 = stmts RBRACE ELSE LBRACE s2 = stmts RBRACE
    { Sif (c, s1, s2) }
| IF LP c = expr RP LBRACE s1 = stmts RBRACE
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

ident:r
| id = IDENT { { loc = ($startpos, $endpos); id } }
;