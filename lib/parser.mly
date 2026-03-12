/* Parser for Markdowns */

%{
  open Ast
%}

/* Tokens */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT

%token TFLOAT TBOOL TSTRING TABBR
%token IF ELSE
%token AND OR NOT

%token EQUAL
%token PLUS MINUS TIMES DIV
%token LP RP LBRACE RBRACE
%token SEMI COMMA

%token OPEN_CODE CLOSE_CODE
%token ANNOTATION
%token <string> TEXT

%token EOF

/* Precedence */
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
| doc = document EOF { doc }
;

document:
| /* empty */                 { [] }
| b = block rest = document   { b :: rest }
;

block:
| OPEN_CODE s = stmts CLOSE_CODE                 { Script s }
| ANNOTATION tag = ident LP arg = ident RP       { Annotation (tag, arg) }

/* Handles: §/ if (expr) { /§ [markdown] §/ } else { /§ [markdown] §/ } /§ */
| OPEN_CODE IF LP c = expr RP LBRACE CLOSE_CODE
  d1 = document
  OPEN_CODE RBRACE ELSE LBRACE CLOSE_CODE
  d2 = document
  OPEN_CODE RBRACE CLOSE_CODE                    { IfBlock (c, d1, d2) }

/* Handles: §/ if (expr) { /§ [markdown] §/ } /§ (No Else branch) */
| OPEN_CODE IF LP c = expr RP LBRACE CLOSE_CODE
  d1 = document
  OPEN_CODE RBRACE CLOSE_CODE                    { IfBlock (c, d1, []) }

| text = raw_text                                { RawText text }
;

/* Left-recursive concatenation of adjacent text tokens */
raw_text:
| t = TEXT                      { t }
| rest = raw_text t = TEXT      { rest ^ t }
;

stmts:
| /* empty */           { [] }
| s = stmt rest = stmts { s :: rest }
;

stmt:
| t = typ id = ident EQUAL e = expr SEMI { Svardef (t, id, e) }
;

typ:
| TFLOAT  { TFloat }
| TBOOL   { TBool }
| TSTRING { TString }
| TABBR   { TAbbr }
;

expr:
| c = CST                         { Ec c }
| id = ident                      { Eident id }
| id = ident LP args = separated_list(COMMA, expr) RP { Ecall (id, args) } /* Parses today() */
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
