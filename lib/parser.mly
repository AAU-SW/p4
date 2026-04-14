%{
  open Ptree
  let mk_loc start_pos end_pos node = { node; ident = (start_pos, end_pos) }
%}

%token <string> MARKDOWN_TEXT IDENT STRING_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token T_INT T_FLOAT T_BOOL T_STRING ABBR IF ELSE
%token ASSIGN EQ_OP GT_OP GTEQ_OP LTEQ_OP LT_OP
%token SEMI LPAREN RPAREN LBRACE IF_END
%token CODE_START CODE_END TEMPLATE_START TEMPLATE_END ANNOTATION_START ANNOTATION_END EOF

%left EQ_OP GT_OP GTEQ_OP LTEQ_OP LT_OP

%start <Ptree.program> program
%%

program:
  | block_elements EOF { $1 }

block_elements:
  | /* empty */ { [] }
  | block_element block_elements { $1 :: $2 }

block_element:
  | MARKDOWN_TEXT { mk_loc $startpos $endpos (Markdown $1) }
  | TEMPLATE_START IDENT TEMPLATE_END { mk_loc $startpos $endpos (Template $2) }
  | ANNOTATION_START IDENT LPAREN IDENT RPAREN ANNOTATION_END { mk_loc $startpos $endpos (Annotation ($2, [$4])) }
  | CODE_START stmts CODE_END { mk_loc $startpos $endpos (CodeBlock $2) }

  /* These rules now use left-factored recursion to resolve LR(1) lookahead conflicts */
  | CODE_START IF LPAREN expr RPAREN LBRACE CODE_END if_body
      {
        let (then_br, else_br) = $8 in
        mk_loc $startpos $endpos (IfElse ($4, then_br, else_br))
      }

/* Specialized right-recursive lists to handle if/else block closures */
if_body:
  | CODE_START IF_END CODE_END { ([], None) }
  | CODE_START IF_END ELSE LBRACE CODE_END if_else_body { ([], Some $6) }
  | block_element if_body {
      let (then_stmts, else_opt) = $2 in
      ($1 :: then_stmts, else_opt)
    }

if_else_body:
  | CODE_START IF_END CODE_END { [] }
  | block_element if_else_body { $1 :: $2 }

stmts:
  | /* empty */ { [] }
  | stmt stmts { $1 :: $2 }

stmt:
  | mds_type IDENT ASSIGN expr SEMI { mk_loc $startpos $endpos (SVarDecl ($1, $2, $4)) }
  | ABBR IDENT ASSIGN STRING_LIT SEMI { mk_loc $startpos $endpos (SAbbrDecl ($2, $4)) }

mds_type:
  | T_INT { TyInt }
  | T_FLOAT { TyFloat }
  | T_BOOL { TyBool }
  | T_STRING { TyString }

expr:
  | INT_LIT { mk_loc $startpos $endpos (EInt $1) }
  | FLOAT_LIT { mk_loc $startpos $endpos (EFloat $1) }
  | BOOL_LIT { mk_loc $startpos $endpos (EBool $1) }
  | STRING_LIT { mk_loc $startpos $endpos (EString $1) }
  | IDENT { mk_loc $startpos $endpos (EVar $1) }
  | IDENT LPAREN RPAREN { mk_loc $startpos $endpos (ECall ($1, [])) }
  | expr EQ_OP expr { mk_loc $startpos $endpos (EBinop (Eq, $1, $3)) }
  | expr GT_OP expr { mk_loc $startpos $endpos (EBinop (Gt, $1, $3)) }
  | expr GTEQ_OP expr { mk_loc $startpos $endpos (EBinop (GtEq, $1, $3)) }
  | expr LTEQ_OP expr { mk_loc $startpos $endpos (EBinop (LtEq, $1, $3)) }
  | expr LT_OP expr { mk_loc $startpos $endpos (EBinop (Lt, $1, $3)) }
