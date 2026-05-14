open Alcotest
open Markdowns_lib

let pp_token fmt = function
  | Parser.MARKDOWN_TEXT s -> Format.fprintf fmt "MARKDOWN_TEXT %S" s
  | Parser.IDENT s -> Format.fprintf fmt "IDENT %S" s
  | Parser.STRING_LIT s -> Format.fprintf fmt "STRING_LIT %S" s
  | Parser.INT_LIT i -> Format.fprintf fmt "INT_LIT %d" i
  | Parser.FLOAT_LIT f -> Format.fprintf fmt "FLOAT_LIT %g" f
  | Parser.BOOL_LIT b -> Format.fprintf fmt "BOOL_LIT %b" b
  | Parser.T_INT -> Format.pp_print_string fmt "T_INT"
  | Parser.T_FLOAT -> Format.pp_print_string fmt "T_FLOAT"
  | Parser.T_BOOL -> Format.pp_print_string fmt "T_BOOL"
  | Parser.T_STRING -> Format.pp_print_string fmt "T_STRING"
  | Parser.ABBR -> Format.pp_print_string fmt "ABBR"
  | Parser.IF -> Format.pp_print_string fmt "IF"
  | Parser.ELSE -> Format.pp_print_string fmt "ELSE"
  | Parser.ASSIGN -> Format.pp_print_string fmt "ASSIGN"
  | Parser.EQ_OP -> Format.pp_print_string fmt "EQ_OP"
  | Parser.GT_OP -> Format.pp_print_string fmt "GT_OP"
  | Parser.GTEQ_OP -> Format.pp_print_string fmt "GTEQ_OP"
  | Parser.LTEQ_OP -> Format.pp_print_string fmt "LTEQ_OP"
  | Parser.LT_OP -> Format.pp_print_string fmt "LT_OP"
  | Parser.SEMI -> Format.pp_print_string fmt "SEMI"
  | Parser.LPAREN -> Format.pp_print_string fmt "LPAREN"
  | Parser.RPAREN -> Format.pp_print_string fmt "RPAREN"
  | Parser.LBRACE -> Format.pp_print_string fmt "LBRACE"
  | Parser.IF_END -> Format.pp_print_string fmt "IF_END"
  | Parser.CODE_START -> Format.pp_print_string fmt "CODE_START"
  | Parser.CODE_END -> Format.pp_print_string fmt "CODE_END"
  | Parser.TEMPLATE_START -> Format.pp_print_string fmt "TEMPLATE_START"
  | Parser.TEMPLATE_END -> Format.pp_print_string fmt "TEMPLATE_END"
  | Parser.ANNOTATION_START -> Format.pp_print_string fmt "ANNOTATION_START"
  | Parser.ANNOTATION_END -> Format.pp_print_string fmt "ANNOTATION_END"
  | Parser.EOF -> Format.pp_print_string fmt "EOF"

let token = Alcotest.testable pp_token ( = )

let tokens_of_string input =
  let lexbuf = Lexing.from_string input in
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []

(* Plain markdown text produces a single MARKDOWN_TEXT token — verifies the
   default mode is Markdown and the greedy rule consumes the whole string. *)
let test_markdown_mode () =
  let result = tokens_of_string "# Header" in
  check (list token) "markdown token stream"
    [Parser.MARKDOWN_TEXT "# Header"; Parser.EOF]
    result

(* ${ switches to Template mode, } switches back — verifies the mode
   transition in and out of a template block works correctly. *)
let test_template_mode () =
  let result = tokens_of_string "${name}" in
  check (list token) "template token stream"
    [Parser.TEMPLATE_START; Parser.IDENT "name"; Parser.TEMPLATE_END; Parser.EOF]
    result

(* $/ switches to Code mode where keywords, identifiers, operators, and
   literals are all recognised; /$ switches back to Markdown mode. *)
let test_code_mode () =
  let result = tokens_of_string "$/ Int x = 3; /$" in
  check (list token) "code token stream"
    [ Parser.CODE_START;
      Parser.T_INT;
      Parser.IDENT "x";
      Parser.ASSIGN;
      Parser.INT_LIT 3;
      Parser.SEMI;
      Parser.CODE_END;
      Parser.EOF ]
    result

(* $$ switches to Annotation mode; a newline terminates the annotation and
   switches back to Markdown mode (newline is the only valid exit). *)
let test_annotation_mode () =
  let result = tokens_of_string "$$ Req(API)\n" in
  check (list token) "annotation token stream"
    [ Parser.ANNOTATION_START;
      Parser.IDENT "Req";
      Parser.LPAREN;
      Parser.IDENT "API";
      Parser.RPAREN;
      Parser.ANNOTATION_END;
      Parser.EOF ]
    result

(* A backtick switches to VerbatimInline mode where $ is not special —
   content passes through as MARKDOWN_TEXT including the surrounding backticks. *)
let test_verbatim_inline_mode () =
  let result = tokens_of_string "`code`" in
  check (list token) "verbatim inline token stream"
    [ Parser.MARKDOWN_TEXT "`";
      Parser.MARKDOWN_TEXT "code";
      Parser.MARKDOWN_TEXT "`";
      Parser.EOF ]
    result

(* Triple backticks switch to VerbatimBlock mode; everything inside including
   newlines is emitted as MARKDOWN_TEXT, and ``` switches back to Markdown mode. *)
let test_verbatim_block_mode () =
  let result = tokens_of_string "```\ncode\n```" in
  check (list token) "verbatim block token stream"
    [ Parser.MARKDOWN_TEXT "```";
      Parser.MARKDOWN_TEXT "\n";
      Parser.MARKDOWN_TEXT "code";
      Parser.MARKDOWN_TEXT "\n";
      Parser.MARKDOWN_TEXT "```";
      Parser.EOF ]
    result

let () =
  Alcotest.run "Lexer Tests" [
    "tokenization", [
      Alcotest.test_case "markdown mode" `Quick test_markdown_mode;
      Alcotest.test_case "template mode" `Quick test_template_mode;
      Alcotest.test_case "code mode" `Quick test_code_mode;
      Alcotest.test_case "annotation mode" `Quick test_annotation_mode;
      Alcotest.test_case "verbatim inline mode" `Quick test_verbatim_inline_mode;
      Alcotest.test_case "verbatim block mode" `Quick test_verbatim_block_mode;
    ];
  ]
