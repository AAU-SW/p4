
open Markdowns_lib (* This is the library name from your lib/dune file *)

let () =
  (* 1. Check if the user provided a filename *)
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <filename.mds>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in

  (* Set the filename in the lexbuf for accurate error reporting *)
  Lexing.set_filename lexbuf filename;

  try
    (* 2. Parse the file into an Abstract Syntax Tree (AST) *)
    let ast = Parser.program Lexer.token lexbuf in

    (* 3. Typecheck the AST *)
    let _typed_ast = Typing.typecheck ast in

    (* 4. Success! *)
    Printf.printf "🎉 Success! '%s' parsed and typechecked with no errors.\n" filename;
    close_in in_channel

  with
  (* Catch Syntax Errors from Menhir *)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "❌ Syntax error in %s at line %d, column %d\n"
        filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      close_in in_channel;
      exit 1

  (* Catch Type Errors from your Typing module *)
  | Typing.TypeError (msg, (start_pos, _)) ->
      Printf.eprintf "❌ Type error in %s at line %d: %s\n"
        filename start_pos.pos_lnum msg;
      close_in in_channel;
      exit 1

  (* Catch anything else (e.g., file not found) *)
  | e ->
      close_in in_channel;
      raise e
