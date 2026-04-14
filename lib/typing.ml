open Ptree
open Ttree

(* Custom exception for type errors, carrying the location for error reporting *)
exception TypeError of string * Ptree.location

(* The environment tracks defined variables (with their types) and glossary abbreviations *)
type env = {
  vars: (string * Ttree.mds_type) list;
  abbrs: string list;
}

let empty_env = { vars = []; abbrs = [] }

(* Helper to format types for error messages *)
let type_to_string = function
  | TyInt -> "Int"
  | TyFloat -> "Float"
  | TyBool -> "Bool"
  | TyString -> "String"

(* --- Expressions --- *)
let rec type_expr env (e : Ptree.expr) : Ttree.typed_expr =
  let ident = e.ident in
  match e.node with
  | EInt i -> { node = TEInt i; ty = TyInt }
  | EFloat f -> { node = TEFloat f; ty = TyFloat }
  | EBool b -> { node = TEBool b; ty = TyBool }
  | EString s -> { node = TEString s; ty = TyString }
  | EVar name ->
      (match List.assoc_opt name env.vars with
       | Some ty -> { node = TEVar name; ty }
       | None -> raise (TypeError ("Unbound variable: " ^ name, ident)))
  | EBinop (op, e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in

      (* Enforce: "Comparisons need to be done between two values of equal type" *)
      if te1.ty <> te2.ty then begin
        let msg = Printf.sprintf "Type mismatch: Cannot compare %s with %s"
                    (type_to_string te1.ty) (type_to_string te2.ty) in
        raise (TypeError (msg, ident))
      end;

      (match op with
        | Eq -> { node = TEBinop (op, te1, te2); ty = TyBool }
        | Gt | GtEq | Lt | LtEq ->
          (* Restrict mathematical comparisons to numbers *)
          if te1.ty = TyInt || te1.ty = TyFloat then
            { node = TEBinop (op, te1, te2); ty = TyBool }
          else
            raise (TypeError ("Relational operators (>, <, >=, <=) require Int or Float", ident)))
  | ECall (name, args) ->
      (* Built-in function handling *)
      if name = "Today" && args = [] then
        { node = TECall (name, []); ty = TyInt }
      else
        raise (TypeError ("Unknown function or invalid arguments: " ^ name, ident))

(* --- Statements --- *)
(* Statements can modify the environment, so they return (new_env, typed_stmt) *)
let type_stmt env (s : Ptree.stmt) : env * Ttree.typed_stmt =
  let ident = s.ident in
  match s.node with
  | SVarDecl (decl_ty, name, expr) ->
        if List.mem_assoc name env.vars then begin
          raise (TypeError ("Variable already declared (Markdowns variables are immutable): " ^ name, ident))
        end;

        let t_expr = type_expr env expr in
        if decl_ty <> t_expr.ty then begin
          let msg = Printf.sprintf "Cannot assign %s to variable of type %s"
                      (type_to_string t_expr.ty) (type_to_string decl_ty) in
          raise (TypeError (msg, ident))
        end;

        let env' = { env with vars = (name, decl_ty) :: env.vars } in
        (env', TSVarDecl (name, t_expr))

  | SAbbrDecl (name, str) ->
        if List.mem name env.abbrs then begin
          raise (TypeError ("Glossary abbreviation already defined: " ^ name, ident))
        end;

        let env' = { env with abbrs = name :: env.abbrs } in
        (env', TSAbbrDecl (name, str))

(* --- Block Elements --- *)
let rec type_block_element env (b : Ptree.block_element) : env * Ttree.typed_block_element =
  let ident = b.ident in
  match b.node with
  | Markdown m -> (env, TMarkdown m)
  | Template name ->
      (match List.assoc_opt name env.vars with
       | Some ty -> (env, TTemplate (name, ty))
       | None -> raise (TypeError ("Unbound variable in template: " ^ name, ident)))
  | Annotation (name, args) ->
      if name = "Req" then
        (match args with
         | [abbr] ->
             if List.mem abbr env.abbrs then
               (env, TAnnotation (name, args))
             else
               raise (TypeError ("Glossary abbreviation not defined before use: " ^ abbr, ident))
         | _ -> raise (TypeError ("Req annotation takes exactly one argument", ident)))
      else
        (* Allow unknown annotations to pass through without breaking compilation *)
        (env, TAnnotation (name, args))

  | CodeBlock stmts ->
        (* Thread the environment through all statements in the block *)
        let rec check_stmts current_env acc_stmts = function
          | [] -> (current_env, List.rev acc_stmts)
          | s :: ss ->
              let (next_env, t_stmt) = type_stmt current_env s in
              check_stmts next_env (t_stmt :: acc_stmts) ss
        in
        (* Pass [] as the initial accumulator here! *)
        let (new_env, t_stmts) = check_stmts env [] stmts in
        (new_env, TCodeBlock t_stmts)

  | IfElse (cond, then_br, else_br_opt) ->
        let t_cond = type_expr env cond in
        if t_cond.ty <> TyBool then begin
          raise (TypeError ("If condition must evaluate to a Bool", ident))
        end;

      (* We typecheck branches using the current environment.
         Variables declared inside an 'if' block do not leak out into the outer scope! *)
      let (_, t_then) = type_program env then_br in
      let t_else_opt = match else_br_opt with
        | Some else_br ->
            let (_, t_else) = type_program env else_br in
            Some t_else
        | None -> None
      in
      (env, TIfElse (t_cond, t_then, t_else_opt))

and type_program env (p : Ptree.program) : env * Ttree.typed_program =
  let rec loop current_env acc = function
    | [] -> (current_env, List.rev acc)
    | b :: bs ->
        let (next_env, t_b) = type_block_element current_env b in
        loop next_env (t_b :: acc) bs
  in
  (* Pass [] as the initial accumulator here! *)
  loop env [] p

(* --- Main Entry Point --- *)
let typecheck (program : Ptree.program) : Ttree.typed_program =
  (* We discard the final environment and just return the Typed Tree *)
  let (_, typed_ast) = type_program empty_env program in
  typed_ast
