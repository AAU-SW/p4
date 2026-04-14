open Markdowns_lib

(* --- AST Optimization Pass --- *)
(* Recursively merges adjacent Markdown text nodes into a single string block *)
let rec compress_markdown acc (blocks : Ptree.block_element list) =
  match blocks with
  | [] -> List.rev acc
  | { Ptree.node = Ptree.Markdown m1; ident = (loc_start, _) } ::
    { Ptree.node = Ptree.Markdown m2; ident = (_, loc_end) } :: rest ->
      (* Combine the strings and extend the location span *)
      let merged = { Ptree.node = Ptree.Markdown (m1 ^ m2); ident = (loc_start, loc_end) } in
      (* Put the merged node back into the list to catch chains of 3 or more *)
      compress_markdown acc (merged :: rest)
  | b :: rest ->
      (* Recursively compress inside IfElse blocks too *)
      let b' = match b.Ptree.node with
        | Ptree.IfElse (cond, then_br, else_br_opt) ->
            let compressed_then = compress_markdown [] then_br in
            let compressed_else = Option.map (compress_markdown []) else_br_opt in
            { b with node = Ptree.IfElse (cond, compressed_then, compressed_else) }
        | _ -> b
      in
      compress_markdown (b' :: acc) rest

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <filename.mds>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in

  Lexing.set_filename lexbuf filename;

  try
    (* 1. Parse the file into the raw AST *)
    let raw_ast = Parser.program Lexer.token lexbuf in

    (* 2. Compress the AST (merge adjacent text nodes) *)
    let ast = compress_markdown [] raw_ast in

    (* 3. Print the optimized AST *)
    Printer.print_program ast;

    (* 4. Typecheck the AST *)
    let _typed_ast = Typing.typecheck ast in

    Printf.printf "🎉 Success! '%s' parsed and typechecked with no errors.\n" filename;
    close_in in_channel

  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "❌ Syntax error in %s at line %d, column %d\n"
        filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      close_in in_channel;
      exit 1

  | Typing.TypeError (msg, (start_pos, _)) ->
      Printf.eprintf "❌ Type error in %s at line %d: %s\n"
        filename start_pos.pos_lnum msg;
      close_in in_channel;
      exit 1

  | e ->
      close_in in_channel;
      raise e
