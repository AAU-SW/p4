open Ttree

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string

type env = {
  vars: (string * value) list;
}

let empty_env = { vars = [] }

let rec eval_expr env (e : typed_expr) : value =
  match e.node with
  | TEInt i -> VInt i
  | TEFloat f -> VFloat f
  | TEBool b -> VBool b
  | TEString s -> VString s
  | TEVar name ->
      (* Typechecker guarantees this will be found *)
      List.assoc name env.vars
  | TEBinop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      evaluate_binop op v1 v2
  | TECall ("Today", []) ->
      (* Implement your built-in function logic here *)
      VInt (int_of_float (Unix.time ()))
  | TECall _ -> failwith "Unsupported function"

and evaluate_binop op v1 v2 =
  (* Pattern match on op and values to return a VBool *)
  match op, v1, v2 with
  | Eq, VInt i1, VInt i2 -> VBool (i1 = i2)
  | Eq, VFloat f1, VFloat f2 -> VBool (f1 = f2)
  | Eq, VString s1, VString s2 -> VBool (s1 = s2)
  | Eq, VBool b1, VBool b2 -> VBool (b1 = b2)

  | Gt, VInt i1, VInt i2 -> VBool (i1 > i2)
  | Gt, VFloat f1, VFloat f2 -> VBool (f1 > f2)

  | GtEq, VInt i1, VInt i2 -> VBool (i1 >= i2)
  | GtEq, VFloat f1, VFloat f2 -> VBool (f1 >= f2)

  | Lt, VInt i1, VInt i2 -> VBool (i1 < i2)
  | Lt, VFloat f1, VFloat f2 -> VBool (f1 < f2)

  | LtEq, VInt i1, VInt i2 -> VBool (i1 <= i2)
  | LtEq, VFloat f1, VFloat f2 -> VBool (f1 <= f2)
  | _ -> failwith "Invalid operation caught at runtime"

let exec_stmt env (stmt : typed_stmt) : env =
  match stmt with
  | TSVarDecl (name, expr) ->
      let value = eval_expr env expr in
      { vars = (name, value) :: env.vars }
  | TSAbbrDecl (name, str) ->
      env

let rec render_block env (block : typed_block_element) : env * string =
  match block with
  | TMarkdown m ->
      (env, m)

  | TTemplate (name, _) ->
      let value = List.assoc name env.vars in
      let str_val = match value with
        | VInt i -> string_of_int i
        | VFloat f -> string_of_float f
        | VBool b -> string_of_bool b
        | VString s -> s
      in
      (env, str_val)

  | TAnnotation _ ->
      (* Annotations like $$ Req(API) don't output text in the final Markdown *)
      (env, "")

  | TCodeBlock stmts ->
      (* Fold over statements to update the environment.
         Code blocks don't output Markdown, so we return an empty string. *)
      let new_env = List.fold_left exec_stmt env stmts in
      (new_env, "")

  | TIfElse (cond, then_br, else_br_opt) ->
      let cond_val = eval_expr env cond in
      (match cond_val with
       | VBool true ->
           let _, output = render_program env then_br in
           (env, output) (* Scoping: changes inside 'if' don't leak out *)
       | VBool false ->
           (match else_br_opt with
            | Some else_br ->
                let _, output = render_program env else_br in
                (env, output)
            | None -> (env, ""))
       | _ -> failwith "Condition must be a boolean")

and render_program env (blocks : typed_program) : env * string =
  List.fold_left (fun (current_env, acc_str) block ->
    let next_env, block_str = render_block current_env block in
    (next_env, acc_str ^ block_str)
  ) (env, "") blocks

(* Main Entry Point *)
let generate (ast : typed_program) : string =
  let _, final_markdown = render_program empty_env ast in
  final_markdown
