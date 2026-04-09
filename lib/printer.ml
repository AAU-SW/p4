open Ptree

let string_of_type = function
  | TyInt -> "TyInt"
  | TyFloat -> "TyFloat"
  | TyBool -> "TyBool"
  | TyString -> "TyString"

let string_of_binop = function
  | Eq -> "Eq"
  | Gt -> "Gt"
  | GtEq -> "GtEq"
  | Lt -> "Lt"
  | LtEq -> "LtEq"

let rec string_of_expr (e : expr) =
  match e.node with
  | EInt i -> Printf.sprintf "EInt(%d)" i
  | EFloat f -> Printf.sprintf "EFloat(%g)" f
  | EBool b -> Printf.sprintf "EBool(%b)" b
  | EString s -> Printf.sprintf "EString(%S)" s
  | EVar v -> Printf.sprintf "EVar(%S)" v
  | EBinop (op, e1, e2) ->
      Printf.sprintf "EBinop(%s, %s, %s)"
        (string_of_binop op) (string_of_expr e1) (string_of_expr e2)
  | ECall (f, args) ->
      let args_str = String.concat ", " (List.map string_of_expr args) in
      Printf.sprintf "ECall(%S, [%s])" f args_str

let rec string_of_stmt (s : stmt) =
  match s.node with
  | SVarDecl (ty, name, e) ->
      Printf.sprintf "SVarDecl(%s, %S, %s)"
        (string_of_type ty) name (string_of_expr e)
  | SAbbrDecl (name, str) ->
      Printf.sprintf "SAbbrDecl(%S, %S)" name str

let rec string_of_block (b : block_element) =
  match b.node with
  | Markdown m ->
      (* %S automatically wraps the string in quotes and escapes newlines/quotes *)
      Printf.sprintf "Markdown(%S)" m
  | Template t -> Printf.sprintf "Template(%S)" t
  | Annotation (n, args) ->
      let args_str = String.concat "; " (List.map (Printf.sprintf "%S") args) in
      Printf.sprintf "Annotation(%S, [%s])" n args_str
  | CodeBlock stmts ->
      let stmts_str = String.concat ";\n  " (List.map string_of_stmt stmts) in
      Printf.sprintf "CodeBlock([\n  %s\n])" stmts_str
  | IfElse (cond, then_br, else_br_opt) ->
      let then_str = String.concat ",\n  " (List.map string_of_block then_br) in
      let else_str = match else_br_opt with
        | None -> "None"
        | Some e -> Printf.sprintf "Some([\n  %s\n])" (String.concat ",\n  " (List.map string_of_block e))
      in
      Printf.sprintf "IfElse(%s,\n  [\n  %s\n  ],\n  %s\n)"
        (string_of_expr cond) then_str else_str

let print_program (p : program) =
  print_endline "=== AST OUTPUT ===";
  List.iter (fun b -> print_endline (string_of_block b)) p;
  print_endline "=================="
