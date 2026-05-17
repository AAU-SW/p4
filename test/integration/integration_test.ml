open Markdowns_lib

(* ── File reading ────────────────────────────────────────────────── *)

let read_file path = 
  let ic = open_in path in
  Fun.protect 
    ~finally:(fun () -> close_in ic) 
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n; 
      Bytes.to_string s
  )  

let project_root =
  Sys.getcwd ()
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname

let load_fixture name = 
  read_file (project_root ^ "/test/integration/" ^ name)

(* ── Pipeline stage helpers ──────────────────────────────────────── *)

let lex_string source = 
  let lexbuf = Lexing.from_string source in
    let rec loop () =
    let tok = Lexer.token lexbuf in
    if tok <> Parser.EOF then loop ()
  in
  loop ()

let parse_string source =
  let lexbuf = Lexing.from_string source in
  Parser.program Lexer.token lexbuf

let typecheck_string source =
  let ast = parse_string source in
  Typing.typecheck ast

(* ── Reusable test helpers ───────────────────────────────────────── *)

let check_lexes folder label =
  let input = load_fixture (folder ^ "/input.mds") in
  let lexes = try ignore (lex_string input); true
              with _ -> false in
  Alcotest.(check bool) label true lexes

let check_parses folder label =
  let input = load_fixture (folder ^ "/input.mds") in
  let parses = try ignore (parse_string input); true
               with _ -> false in
  Alcotest.(check bool) label true parses

let check_typechecks folder label =
  let input = load_fixture (folder ^ "/input.mds") in
  let typechecks = try ignore (typecheck_string input); true
                   with _ -> false in
  Alcotest.(check bool) label true typechecks

(* ── Variable tests ──────────────────────────────────────────────── *)
let test_variable_template_lexes () =
  check_lexes "variable_template" "Variable template lexes correctly"

let test_variable_template_parses () =
  check_parses "variable_template" "Variable template parses correctly"

let test_variable_template_typechecks () =
  check_typechecks "variable_template" "Variable template typechecks correctly"

(* ── Conditional tests ───────────────────────────────────────────── *)
let test_conditional_template_lexes () =
  check_lexes "conditional_template" "Conditional template lexes correctly"

let test_conditional_template_parses () =
  check_parses "conditional_template" "Conditional template parses correctly"

let test_conditional_template_typechecks () =
  check_typechecks "conditional_template" "Conditional template typechecks correctly"

(* ── Abbreviation tests ───────────────────────────────────────────── *)

let test_abbr_template_lexes () =
  check_lexes "abbr_template" "Abbreviation template lexes correctly"

let test_abbr_template_parses () =
  check_parses "abbr_template" "Abbreviation template parses correctly"

let test_abbr_template_typechecks () =
  check_typechecks "abbr_template" "Abbreviation template typechecks correctly"


let () =
  Alcotest.run "Integration Tests" [
    "variables", [
      Alcotest.test_case "lexes"     `Quick test_variable_template_lexes;
      Alcotest.test_case "parses"     `Quick test_variable_template_parses;
      Alcotest.test_case "typechecks" `Quick test_variable_template_typechecks;
    ];

    "conditionals", [
      Alcotest.test_case "lexes"     `Quick test_conditional_template_lexes;
      Alcotest.test_case "parses"     `Quick test_conditional_template_parses;
      Alcotest.test_case "typechecks" `Quick test_conditional_template_typechecks;
    ];

    "abbreviations", [
      Alcotest.test_case "lexes"     `Quick test_abbr_template_lexes;
      Alcotest.test_case "parses"     `Quick test_abbr_template_parses;
      Alcotest.test_case "typechecks" `Quick test_abbr_template_typechecks;
    ]
  ]