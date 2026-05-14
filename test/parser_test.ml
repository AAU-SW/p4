open Alcotest
open Markdowns_lib

(* Uses the real lexer, so a lexer bug can cause these tests to fail even if
   the parser is correct. For true isolation, the lexer should be mocked. *)
let parse_string input =
  let lexbuf = Lexing.from_string input in
  Parser.program Lexer.token lexbuf

(* Plain text with no special markers produces a single Markdown node
   containing the full string — the baseline for the parser. *)
let test_simple_markdown () =
  let input = "Hello world" in
  let result = parse_string input in
  match result with
  | [{ node = Ptree.Markdown text; _ }] -> check string "markdown text" "Hello world" text
  | _ -> fail "Expected single markdown element"

(* A bare template produces a single Template node with the correct variable name. *)
let test_template () =
  let input = "${varname}" in
  let result = parse_string input in
  match result with
  | [{ node = Ptree.Template name; _ }] -> check string "template name" "varname" name
  | _ -> fail "Expected single template element"

(* An empty code block ($/ /$) produces a CodeBlock node with zero statements. *)
let test_code_block () =
  let input = "$/ /$" in
  let result = parse_string input in
  match result with
  | [{ node = Ptree.CodeBlock stmts; _ }] -> check int "empty code block" 0 (List.length stmts)
  | _ -> fail "Expected single code block"

(* A template embedded in surrounding text produces three nodes in order:
   Markdown, Template, Markdown — verifies correct interleaving. *)
let test_markdown_with_template () =
  let input = "Text ${myvar} more" in
  let result = parse_string input in
  match result with
  | [ { node = Ptree.Markdown "Text "; _ };
      { node = Ptree.Template "myvar"; _ };
      { node = Ptree.Markdown " more"; _ } ] -> ()
  | _ -> fail "Expected Markdown, Template, Markdown nodes"

(* An annotation produces a single Annotation node with the correct name and
   argument — also verifies the annotation is terminated by the newline. *)
let test_annotation () =
  let input = "$$ Req(API)\n" in
  let result = parse_string input in
  match result with
  | [{ node = Ptree.Annotation (name, args); _ }] ->
      check string "annotation name" "Req" name;
      check int "annotation has one arg" 1 (List.length args)
  | _ -> fail "Expected single annotation element"

let () =
  Alcotest.run "Parser Tests" [
    "basic", [
      Alcotest.test_case "simple markdown" `Quick test_simple_markdown;
      Alcotest.test_case "template" `Quick test_template;
      Alcotest.test_case "code block" `Quick test_code_block;
      Alcotest.test_case "markdown with template" `Quick test_markdown_with_template;
      Alcotest.test_case "annotation" `Quick test_annotation;
    ];
  ]
