(* bin/main.ml *)

open Markdowns_lib.Ast
open Printf

(* Translates Types into readable AST node names *)
let string_of_typ = function
  | TFloat -> "TFloat"
  | TBool -> "TBool"
  | TString -> "TString"
  | TAbbr -> "TAbbr"

(* Translates Constants into readable AST node names *)
let string_of_constant = function
  | Cfloat f -> sprintf "Cfloat(%g)" f
  | Cbool b -> sprintf "Cbool(%b)" b
  | Cstring s -> sprintf "Cstring(\"%s\")" s

(* Translates Binary Operators into readable AST node names *)
let string_of_binop = function
  | Badd -> "Badd" | Bsub -> "Bsub" | Bmul -> "Bmul" | Bdiv -> "Bdiv" | Bmod -> "Bmod"
  | Beq -> "Beq" | Bneq -> "Bneq" | Blt -> "Blt" | Ble -> "Ble" | Bgt -> "Bgt" | Bge -> "Bge"
  | Band -> "Band" | Bor -> "Bor"

(* Deeply prints Expressions as AST nodes *)
let rec string_of_expr indent = function
  | Ec c -> sprintf "%sEc(%s)" indent (string_of_constant c)
  | Eident id -> sprintf "%sEident(\"%s\")" indent id.id
  | Ebinop (op, e1, e2) ->
      let next_indent = indent ^ "  " in
      sprintf "%sEbinop(\n%s%s,\n%s,\n%s\n%s)"
        indent next_indent (string_of_binop op) (string_of_expr next_indent e1) (string_of_expr next_indent e2) indent
  | Eunop (Unot, e) ->
      let next_indent = indent ^ "  " in
      sprintf "%sEunop(Unot,\n%s\n%s)" indent (string_of_expr next_indent e) indent
  | Ecall (id, args) ->
      let next_indent = indent ^ "  " in
      let args_str = String.concat ",\n" (List.map (string_of_expr next_indent) args) in
      if args = [] then
        sprintf "%sEcall(\"%s\", [])" indent id.id
      else
        sprintf "%sEcall(\"%s\", [\n%s\n%s])" indent id.id args_str indent

(* Deeply prints Statements as AST nodes *)
let string_of_stmt indent = function
  | Svardef (t, id, e) ->
      let next_indent = indent ^ "  " in
      sprintf "%sSvardef(%s, \"%s\",\n%s\n%s)"
        indent (string_of_typ t) id.id (string_of_expr next_indent e) indent

(* Forward declarations for mutually recursive printing functions *)
let rec print_document indent doc =
  List.iter (fun el -> printf "%s%s\n" indent (string_of_doc_element indent el)) doc

(* Deeply prints Top-level document blocks as AST nodes *)
and string_of_doc_element indent = function
  | RawText t -> sprintf "RawText(\"%s\")" (String.escaped t)
  | Annotation (tag, arg) -> sprintf "Annotation(Tag: \"%s\", Arg: \"%s\")" tag.id arg.id
  | Script stmts ->
      let next_indent = indent ^ "  " in
      let stmts_str = String.concat ",\n" (List.map (string_of_stmt next_indent) stmts) in
      if stmts = [] then
        sprintf "Script([])"
      else
        sprintf "Script([\n%s\n%s])" stmts_str indent
  | IfBlock (cond, d1, d2) ->
      let next_indent = indent ^ "  " in
      let cond_str = string_of_expr next_indent cond in

      let buf1 = Buffer.create 256 in
      let buf2 = Buffer.create 256 in
      let doc_indent = next_indent ^ "  " in

      List.iter (fun el -> Buffer.add_string buf1 (sprintf "%s%s\n" doc_indent (string_of_doc_element doc_indent el))) d1;
      List.iter (fun el -> Buffer.add_string buf2 (sprintf "%s%s\n" doc_indent (string_of_doc_element doc_indent el))) d2;

      if List.length d2 = 0 then
        sprintf "IfBlock(\n%s,\n%s  Then([\n%s%s  ])\n%s)"
          cond_str indent (Buffer.contents buf1) indent indent
      else
        sprintf "IfBlock(\n%s,\n%s  Then([\n%s%s  ]),\n%s  Else([\n%s%s  ])\n%s)"
          cond_str indent (Buffer.contents buf1) indent indent (Buffer.contents buf2) indent indent

let () =
  if Array.length Sys.argv < 2 then begin
    eprintf "Usage: %s <file.mds>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  try
    let ast = Markdowns_lib.Parser.file Markdowns_lib.Lexer.next_token lexbuf in
    close_in chan;

    printf "Successfully parsed '%s'.\n\nAST Output:\n[\n" filename;
    print_document "  " ast;
    printf "]\n"

  with
  | Markdowns_lib.Lexer.Lexing_error msg ->
      eprintf "Lexing error: %s\n" msg;
      close_in chan; exit 1
  | Markdowns_lib.Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      eprintf "Syntax error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      close_in chan; exit 1
