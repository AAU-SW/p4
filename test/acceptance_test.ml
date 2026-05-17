(** Acceptance tests for the Markdowns compiler.

    These tests treat the compiler as a black box: a .mds source string goes
    in and either a rendered Markdown string comes out, or a compile-time error
    is raised. No internal module is inspected directly.
*)

open Alcotest
open Markdowns_lib

(** Reset the lexer's global mode to Markdown before each parse.
    The lexer uses a mutable current_mode ref; if a previous test left it in
    Code or Annotation mode (e.g. because parsing aborted mid-way), the next
    test would start in the wrong mode and misidentify tokens. *)
let reset_lexer () = Lexer.current_mode := Lexer.Markdown

(** Run the full compiler pipeline on src and return the rendered output.
    Mirrors bin/main.ml: parse -> compress -> typecheck -> codegen. *)
let compile src =
  reset_lexer ();
  let lexbuf = Lexing.from_string src in
  let raw_ast = Parser.program Lexer.token lexbuf in
  (* Reproduce the compress_markdown pass from bin/main.ml *)
  let rec compress acc = function
    | [] -> List.rev acc
    | { Ptree.node = Ptree.Markdown m1; ident = (s, _) } ::
      { Ptree.node = Ptree.Markdown m2; ident = (_, e) } :: rest ->
        let merged = { Ptree.node = Ptree.Markdown (m1 ^ m2);
                       Ptree.ident = (s, e) } in
        compress acc (merged :: rest)
    | b :: rest ->
        let b' = match b.Ptree.node with
          | Ptree.IfElse (cond, tb, eb) ->
              { b with Ptree.node =
                  Ptree.IfElse (cond, compress [] tb,
                                Option.map (compress []) eb) }
          | _ -> b
        in
        compress (b' :: acc) rest
  in
  let ast = compress [] raw_ast in
  let typed = Typing.typecheck ast in
  Codegen.generate typed

(** Assert that compiling src raises a Typing.TypeError. *)
let assert_type_error src =
  reset_lexer ();
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  match Typing.typecheck ast with
  | _ -> fail "Expected a TypeError but compilation succeeded"
  | exception Typing.TypeError _ -> ()

(** Assert that compiling src raises a parse or lex error.
    The lexer raises Failure for illegal input (e.g. "lexing: empty token")
    while the parser raises Parser.Error for structural errors. Both are
    valid signals that the source is syntactically invalid. *)
let assert_syntax_error src =
  reset_lexer ();
  let lexbuf = Lexing.from_string src in
  match Parser.program Lexer.token lexbuf with
  | _ -> fail "Expected a syntax error but parsing succeeded"
  | exception Parser.Error -> ()
  | exception Failure _ -> ()

(** Trim leading/trailing whitespace from every line, then the whole string.
    Lets tests stay readable without caring about indentation. *)
let normalize s =
  s
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun l -> l <> "")
  |> String.concat "\n"

(** Substring check used by end-to-end tests. *)
let contains sub s =
  let sl = String.length s and subl = String.length sub in
  let rec loop i =
    if i + subl > sl then false
    else if String.sub s i subl = sub then true
    else loop (i + 1)
  in
  loop 0


(* ------------------------------------------------------------------ *)
(* 1. Plain Markdown passthrough                                        *)
(* ------------------------------------------------------------------ *)

(** Markdown text that contains no MDS constructs must be emitted verbatim. *)
let test_plain_markdown () =
  let src = "# Hello\n\nThis is plain markdown." in
  check string "plain markdown passthrough"
    src
    (String.trim (compile src))

(** Multiple consecutive markdown lines are preserved (the compress pass
    merges adjacent nodes but must not drop newlines). *)
let test_multiline_markdown () =
  let src = "line one\nline two\nline three" in
  let output = compile src in
  check string "multiline preserved"
    src
    (String.trim output)


(* ------------------------------------------------------------------ *)
(* 2. Variables — declaration and template interpolation               *)
(* ------------------------------------------------------------------ *)

let test_string_variable () =
  let output = compile {|
$/
String project = "Markdowns";
/$
Project: ${project}
|} in
  check bool "string interpolated"
    true (String.trim output |> fun s -> String.length s > 0
                                         && let last = String.sub s
                                              (String.length s - String.length "Markdowns")
                                              (String.length "Markdowns") in
                                            last = "Markdowns")

let test_int_variable () =
  let output = normalize (compile {|
$/
Int version = 42;
/$
v${version}
|}) in
  check string "int variable renders without decimal"
    "v42" output

let test_float_variable () =
  let output = normalize (compile {|
$/
Float ratio = 0.5;
/$
ratio: ${ratio}
|}) in
  (* string_of_float in OCaml produces "0.5" for 0.5 *)
  check string "float variable renders"
    "ratio: 0.5" output

let test_bool_variable_true () =
  let output = normalize (compile {|
$/
Bool flag = true;
/$
flag: ${flag}
|}) in
  check string "bool true renders as 'true'"
    "flag: true" output

let test_bool_variable_false () =
  let output = normalize (compile {|
$/
Bool flag = false;
/$
flag: ${flag}
|}) in
  check string "bool false renders as 'false'"
    "flag: false" output

(** Variables are immutable — re-declaring the same name is a type error. *)
let test_variable_immutability () =
  assert_type_error {|
$/
Int x = 1;
Int x = 2;
/$
|}

(** A template referencing an undeclared variable is a type error. *)
let test_template_unbound_variable () =
  assert_type_error "${does_not_exist}"

(** A variable must be declared before it is used in a template. *)
let test_variable_must_be_declared_before_use () =
  assert_type_error {|
${name}
$/
String name = "late";
/$
|}


(* ------------------------------------------------------------------ *)
(* 3. Type system                                                       *)
(* ------------------------------------------------------------------ *)

(** Assigning a String literal to an Int variable is a type error. *)
let test_type_mismatch_string_to_int () =
  assert_type_error {|
$/
Int x = "hello";
/$
|}

(** Assigning a Bool literal to a Float variable is a type error. *)
let test_type_mismatch_bool_to_float () =
  assert_type_error {|
$/
Float x = true;
/$
|}

(** Assigning an Int literal to a String variable is a type error. *)
let test_type_mismatch_int_to_string () =
  assert_type_error {|
$/
String s = 99;
/$
|}

(** Comparing an Int to a String with == is a type error. *)
let test_binop_type_mismatch_eq () =
  assert_type_error {|
$/
Int a = 1;
String b = "1";
/$
$/ if (a == b) { /$yes$/ } /$
|}

(** Relational operators on Bool values are a type error. *)
let test_relational_op_on_bool () =
  assert_type_error {|
$/
Bool a = true;
Bool b = false;
/$
$/ if (a > b) { /$yes$/ } /$
|}

(** Relational operators on String values are a type error. *)
let test_relational_op_on_string () =
  assert_type_error {|
$/
String a = "x";
String b = "y";
/$
$/ if (a < b) { /$yes$/ } /$
|}

(** == comparison between two Ints of the same type is legal. *)
let test_eq_same_type_ok () =
  let output = normalize (compile {|
$/
Int a = 1;
Int b = 1;
/$
$/ if (a == b) { /$same$/ } /$
|}) in
  check string "eq same type renders" "same" output


(* ------------------------------------------------------------------ *)
(* 4. Conditionals                                                      *)
(* ------------------------------------------------------------------ *)

(** When condition is true, the then-branch renders; else-branch does not. *)
let test_if_true_renders_then () =
  let output = normalize (compile {|
$/
Bool ready = true;
/$
$/ if (ready == true) { /$
yes
$/ } else { /$
no
$/ } /$
|}) in
  check string "if true -> then branch" "yes" output

(** When condition is false, the else-branch renders; then-branch does not. *)
let test_if_false_renders_else () =
  let output = normalize (compile {|
$/
Bool ready = false;
/$
$/ if (ready == true) { /$
yes
$/ } else { /$
no
$/ } /$
|}) in
  check string "if false -> else branch" "no" output

(** An if without an else, with a false condition, renders nothing. *)
let test_if_false_no_else_renders_nothing () =
  let output = normalize (compile {|
$/
Bool flag = false;
/$
before
$/ if (flag == true) { /$
hidden
$/ } /$
after
|}) in
  check string "if false no else -> empty" "before\nafter" output

(** Numeric > comparison: render then-branch when condition holds. *)
let test_if_gt_true () =
  let output = normalize (compile {|
$/
Int a = 10;
Int b = 5;
/$
$/ if (a > b) { /$bigger$/ } else { /$smaller$/ } /$
|}) in
  check string "gt true" "bigger" output

(** Numeric >= comparison at the boundary. *)
let test_if_gteq_equal () =
  let output = normalize (compile {|
$/
Int a = 7;
Int b = 7;
/$
$/ if (a >= b) { /$ok$/ } else { /$fail$/ } /$
|}) in
  check string "gteq equal boundary" "ok" output

(** Numeric <= comparison. *)
let test_if_lteq_true () =
  let output = normalize (compile {|
$/
Int x = 3;
Int y = 5;
/$
$/ if (x <= y) { /$yes$/ } else { /$no$/ } /$
|}) in
  check string "lteq true" "yes" output

(** Numeric < comparison where it is false. *)
let test_if_lt_false () =
  let output = normalize (compile {|
$/
Int x = 10;
Int y = 5;
/$
$/ if (x < y) { /$yes$/ } else { /$no$/ } /$
|}) in
  check string "lt false" "no" output

(** String == comparison. *)
let test_if_string_eq () =
  let output = normalize (compile {|
$/
String env = "prod";
/$
$/ if (env == "prod") { /$production$/ } else { /$other$/ } /$
|}) in
  check string "string eq true" "production" output

(** Bool == comparison with false literal. *)
let test_if_bool_eq_false_literal () =
  let output = normalize (compile {|
$/
Bool done_ = false;
/$
$/ if (done_ == false) { /$not done$/ } else { /$done$/ } /$
|}) in
  check string "bool eq false literal" "not done" output

(** If condition must be Bool — Int condition is a type error. *)
let test_if_condition_must_be_bool () =
  assert_type_error {|
$/
Int n = 1;
/$
$/ if (n) { /$yes$/ } /$
|}

(** Variables declared inside an if-block are not visible outside it.
    This is the scoping rule: inner declarations do not leak. *)
let test_if_scope_does_not_leak () =
  assert_type_error {|
$/
Bool flag = true;
/$
$/ if (flag == true) { /$
$/
String inner = "secret";
/$
$/ } /$
${inner}
|}

(** Templates and variables work correctly inside a rendered if-branch. *)
let test_template_inside_if () =
  let output = normalize (compile {|
$/
Bool show = true;
String msg = "hello";
/$
$/ if (show == true) { /$
${msg}
$/ } /$
|}) in
  check string "template inside if renders" "hello" output


(* ------------------------------------------------------------------ *)
(* 5. Glossary (Abbr / Req)                                            *)
(* ------------------------------------------------------------------ *)

(** Abbr declarations and Req annotations produce no output text. *)
let test_abbr_and_req_produce_no_output () =
  let output = normalize (compile {|
$/
Abbr API = "Application Programming Interface";
/$
before
$$ Req(API)
after
|}) in
  check string "abbr and req produce no output" "before\nafter" output

(** Using Req before the abbreviation is defined is a type error. *)
let test_req_before_abbr_definition () =
  assert_type_error {|
$$ Req(UNKNOWN)
$/
Abbr UNKNOWN = "Something";
/$
|}

(** Req on a name that is never declared anywhere is a type error. *)
let test_req_undefined_abbreviation () =
  assert_type_error "$$ Req(GHOST)\n"

(** Redeclaring the same Abbr name is a type error. *)
let test_abbr_redeclaration () =
  assert_type_error {|
$/
Abbr API = "Application Programming Interface";
Abbr API = "Another thing";
/$
|}

(** Multiple different abbreviations can be defined and used. *)
let test_multiple_abbrs () =
  let output = normalize (compile {|
$/
Abbr API = "Application Programming Interface";
Abbr CI  = "Continuous Integration";
/$
start
$$ Req(API)
middle
$$ Req(CI)
end
|}) in
  check string "multiple abbrs, no output" "start\nmiddle\nend" output


(* ------------------------------------------------------------------ *)
(* 6. Built-in functions                                               *)
(* ------------------------------------------------------------------ *)

(** Today() returns an Int (unix timestamp). We can't check the exact value,
    but we can verify it compiles and the result is usable as an Int. *)
let test_today_returns_int () =
  let output = normalize (compile {|
$/
Int now = Today();
Int past = 1000000000;
/$
$/ if (now > past) { /$future$/ } else { /$past$/ } /$
|}) in
  (* Any date after year 2001 satisfies now > 1000000000 *)
  check string "Today() > year-2001 timestamp" "future" output

(** Today() assigned to a Float variable is a type error. *)
let test_today_assigned_to_wrong_type () =
  assert_type_error {|
$/
Float t = Today();
/$
|}

(** Calling an unknown function is a type error. *)
let test_unknown_function () =
  assert_type_error {|
$/
Int x = Foo();
/$
|}


(* ------------------------------------------------------------------ *)
(* 7. Verbatim blocks (code fences)                                    *)
(* ------------------------------------------------------------------ *)

(** Content inside triple-backtick fences is passed through unchanged —
    $ characters inside are NOT interpreted as MDS syntax. *)
let test_verbatim_block_not_interpreted () =
  let src = "```\n$/ Int x = 1; /$\n```" in
  let output = String.trim (compile src) in
  check string "verbatim block passthrough" src output

(** Content inside a backtick inline span is passed through unchanged. *)
let test_verbatim_inline_not_interpreted () =
  let src = "`${variable}`" in
  let output = String.trim (compile src) in
  check string "verbatim inline passthrough" src output


(* ------------------------------------------------------------------ *)
(* 8. Markdown compression (adjacent node merging)                     *)
(* ------------------------------------------------------------------ *)

(** Adjacent markdown text segments are merged into a single node.
    The observable effect is that no content is lost or duplicated. *)
let test_adjacent_markdown_merged () =
  let output = String.trim (compile "foo\nbar\nbaz") in
  check string "adjacent markdown merged correctly"
    "foo\nbar\nbaz" output


(* ------------------------------------------------------------------ *)
(* 9. Syntax errors                                                     *)
(* ------------------------------------------------------------------ *)

(** A code block with no closing /$ is a syntax error. *)
let test_unclosed_code_block () =
  assert_syntax_error "$/ Int x = 1;"

(** A template with no closing } is a syntax error. *)
let test_unclosed_template () =
  assert_syntax_error "${unclosed"

(** A variable declaration missing the semicolon is a syntax error. *)
let test_missing_semicolon () =
  assert_syntax_error "$/ Int x = 1 /$"


(* ------------------------------------------------------------------ *)
(* 10. End-to-end: realistic documents                                  *)
(* ------------------------------------------------------------------ *)

(** A realistic document combining variables, conditionals, glossary,
    templates, and plain markdown all in one source. *)
let test_realistic_document () =
  (* Use raw output for substring checks — normalize() would collapse
     multi-word strings that span adjacent markdown nodes. *)
  let output = compile {|
$/
String title = "Release Notes";
Int version = 2;
Bool released = true;
Abbr API = "Application Programming Interface";
/$
# ${title}

Version ${version}

$$ Req(API)
Built on top of the API.

$/ if (released == true) { /$
Status: live
$/ } else { /$
Status: pending
$/ } /$
|} in
  check bool "title rendered"      true  (contains "Release Notes" output);
  check bool "version rendered"    true  (contains "Version 2" output);
  check bool "api text rendered"   true  (contains "Built on top of the API" output);
  check bool "live branch visible" true  (contains "Status: live" output);
  check bool "pending not visible" false (contains "Status: pending" output)

(** Float comparison: a document that uses a float variable in a conditional. *)
let test_float_comparison_document () =
  (* The output contains surrounding newlines from adjacent markdown/code nodes;
     use a substring check rather than exact equality after normalization. *)
  let output = compile {|
$/
Float threshold = 0.5;
Float value = 0.9;
/$
$/ if (value > threshold) { /$above$/ } else { /$below$/ } /$
|} in
  check bool "above rendered" true  (contains "above" output);
  check bool "below hidden"   false (contains "below" output)


(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Acceptance Tests" [

    "plain markdown", [
      test_case "passthrough"          `Quick test_plain_markdown;
      test_case "multiline"            `Quick test_multiline_markdown;
    ];

    "variables", [
      test_case "string"               `Quick test_string_variable;
      test_case "int"                  `Quick test_int_variable;
      test_case "float"                `Quick test_float_variable;
      test_case "bool true"            `Quick test_bool_variable_true;
      test_case "bool false"           `Quick test_bool_variable_false;
      test_case "immutability"         `Quick test_variable_immutability;
      test_case "unbound in template"  `Quick test_template_unbound_variable;
      test_case "declare before use"   `Quick test_variable_must_be_declared_before_use;
    ];

    "type system", [
      test_case "string to int"        `Quick test_type_mismatch_string_to_int;
      test_case "bool to float"        `Quick test_type_mismatch_bool_to_float;
      test_case "int to string"        `Quick test_type_mismatch_int_to_string;
      test_case "eq type mismatch"     `Quick test_binop_type_mismatch_eq;
      test_case "relop on bool"        `Quick test_relational_op_on_bool;
      test_case "relop on string"      `Quick test_relational_op_on_string;
      test_case "eq same type ok"      `Quick test_eq_same_type_ok;
    ];

    "conditionals", [
      test_case "if true"              `Quick test_if_true_renders_then;
      test_case "if false"             `Quick test_if_false_renders_else;
      test_case "if false no else"     `Quick test_if_false_no_else_renders_nothing;
      test_case "gt true"              `Quick test_if_gt_true;
      test_case "gteq boundary"        `Quick test_if_gteq_equal;
      test_case "lteq true"            `Quick test_if_lteq_true;
      test_case "lt false"             `Quick test_if_lt_false;
      test_case "string eq"            `Quick test_if_string_eq;
      test_case "bool eq false lit"    `Quick test_if_bool_eq_false_literal;
      test_case "condition not bool"   `Quick test_if_condition_must_be_bool;
      test_case "scope no leak"        `Quick test_if_scope_does_not_leak;
      test_case "template inside if"   `Quick test_template_inside_if;
    ];

    "glossary", [
      test_case "abbr req no output"   `Quick test_abbr_and_req_produce_no_output;
      test_case "req before abbr"      `Quick test_req_before_abbr_definition;
      test_case "req undefined"        `Quick test_req_undefined_abbreviation;
      test_case "abbr redeclaration"   `Quick test_abbr_redeclaration;
      test_case "multiple abbrs"       `Quick test_multiple_abbrs;
    ];

    "built-in functions", [
      test_case "Today returns int"    `Quick test_today_returns_int;
      test_case "Today wrong type"     `Quick test_today_assigned_to_wrong_type;
      test_case "unknown function"     `Quick test_unknown_function;
    ];

    "verbatim", [
      test_case "block not interpreted"  `Quick test_verbatim_block_not_interpreted;
      test_case "inline not interpreted" `Quick test_verbatim_inline_not_interpreted;
    ];

    "markdown compression", [
      test_case "adjacent nodes merged" `Quick test_adjacent_markdown_merged;
    ];

    "syntax errors", [
      test_case "unclosed code block"  `Quick test_unclosed_code_block;
      test_case "unclosed template"    `Quick test_unclosed_template;
      test_case "missing semicolon"    `Quick test_missing_semicolon;
    ];

    "end-to-end", [
      test_case "realistic document"   `Quick test_realistic_document;
      test_case "float comparison"     `Quick test_float_comparison_document;
    ];
  ]
