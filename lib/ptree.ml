type location = Lexing.position * Lexing.position
type 'a ident = { node: 'a; ident: location }

type mds_type =
  | TyFloat
  | TyInt
  | TyBool
  | TyString

type binop =
  | Eq      (* == *)
  | Gt      (* >  *)
  | GtEq    (* >= *)
  | Lt      (* <  *)
  | LtEq    (* <= *)

type expr_node =
  | EInt of int
  | EFloat of float
  | EBool of bool
  | EString of string
  | EVar of string
  | EBinop of binop * expr * expr
  | ECall of string * expr list  (* e.g., Today(), Req() *)

and expr = expr_node ident

type stmt_node =
  | SVarDecl of mds_type * string * expr  (* Float f = 0.0; *)
  | SAbbrDecl of string * string          (* Abbr API = "..."; *)

and stmt = stmt_node ident

type block_element_node =
  | Markdown of string                    (* Standard markdown text *)
  | Template of string                    (* ${var_name} *)
  | Annotation of string * string list    (* $$ Req(API) -> Name, args *)
  | CodeBlock of stmt list                (* $/ ... /$ *)
  | IfElse of expr * block_element list * block_element list option

and block_element = block_element_node ident

type program = block_element list
