type mds_type = Ptree.mds_type
type binop = Ptree.binop

type typed_expr_node =
  | TEInt of int
  | TEFloat of float
  | TEBool of bool
  | TEString of string
  | TEVar of string
  | TEBinop of binop * typed_expr * typed_expr
  | TECall of string * typed_expr list

and typed_expr = {
  node: typed_expr_node;
  ty: mds_type;
}

type typed_stmt =
  | TSVarDecl of string * typed_expr
  | TSAbbrDecl of string * string

type typed_block_element =
  | TMarkdown of string
  | TTemplate of string * mds_type
  | TAnnotation of string * string list
  | TCodeBlock of typed_stmt list
  | TIfElse of typed_expr * typed_block_element list * typed_block_element list option

type typed_program = typed_block_element list
