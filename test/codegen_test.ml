open Alcotest
open Markdowns_lib
open Ttree

(* Constructs a typed_program directly without going through the lexer or
   parser, so these are true unit tests of the codegen in isolation. *)
let generate ast = Codegen.generate ast

(* Plain markdown text is passed through to the output unchanged. *)
let test_markdown_passthrough () =
  let ast = [ TMarkdown "# Hello" ] in
  check string "markdown passthrough" "# Hello" (generate ast)

(* Annotations produce no output — they are metadata only. *)
let test_annotation_no_output () =
  let ast = [ TAnnotation ("Req", ["API"]) ] in
  check string "annotation silent" "" (generate ast)

(* A code block executes and binds variables but emits no output itself. *)
let test_code_block_no_output () =
  let ast = [ TCodeBlock [ TSVarDecl ("x", { node = TEInt 1; ty = TyInt }) ] ] in
  check string "code block silent" "" (generate ast)

(* A variable declared in a code block is interpolated correctly by a
   following template — verifies the runtime environment is threaded through. *)
let test_template_int () =
  let ast = [
    TCodeBlock [ TSVarDecl ("n", { node = TEInt 42; ty = TyInt }) ];
    TTemplate ("n", TyInt)
  ] in
  check string "int template" "42" (generate ast)

(* Float, Bool, and String variables are stringified correctly by templates. *)
let test_template_float () =
  let ast = [
    TCodeBlock [ TSVarDecl ("f", { node = TEFloat 3.14; ty = TyFloat }) ];
    TTemplate ("f", TyFloat)
  ] in
  check string "float template" "3.14" (generate ast)

let test_template_bool () =
  let ast = [
    TCodeBlock [ TSVarDecl ("b", { node = TEBool true; ty = TyBool }) ];
    TTemplate ("b", TyBool)
  ] in
  check string "bool template" "true" (generate ast)

let test_template_string () =
  let ast = [
    TCodeBlock [ TSVarDecl ("s", { node = TEString "hello"; ty = TyString }) ];
    TTemplate ("s", TyString)
  ] in
  check string "string template" "hello" (generate ast)

(* When the condition is true, the then-branch is rendered. *)
let test_if_true () =
  let ast = [
    TIfElse (
      { node = TEBool true; ty = TyBool },
      [ TMarkdown "yes" ],
      Some [ TMarkdown "no" ]
    )
  ] in
  check string "if true renders then" "yes" (generate ast)

(* When the condition is false, the else-branch is rendered. *)
let test_if_false () =
  let ast = [
    TIfElse (
      { node = TEBool false; ty = TyBool },
      [ TMarkdown "yes" ],
      Some [ TMarkdown "no" ]
    )
  ] in
  check string "if false renders else" "no" (generate ast)

(* When the condition is false and there is no else-branch, nothing is rendered. *)
let test_if_false_no_else () =
  let ast = [
    TIfElse (
      { node = TEBool false; ty = TyBool },
      [ TMarkdown "yes" ],
      None
    )
  ] in
  check string "if false no else is empty" "" (generate ast)

(* Variables declared inside an if-branch do not affect output outside it —
   the outer environment is restored after the branch finishes. *)
let test_if_scope_does_not_leak () =
  let ast = [
    TCodeBlock [ TSVarDecl ("x", { node = TEString "outer"; ty = TyString }) ];
    TIfElse (
      { node = TEBool true; ty = TyBool },
      [ TCodeBlock [ TSVarDecl ("inner", { node = TEString "inside"; ty = TyString }) ] ],
      None
    );
    TTemplate ("x", TyString)
  ] in
  check string "outer variable unaffected by if scope" "outer" (generate ast)

let () =
  Alcotest.run "Codegen Tests" [
    "output", [
      Alcotest.test_case "markdown passthrough"    `Quick test_markdown_passthrough;
      Alcotest.test_case "annotation no output"    `Quick test_annotation_no_output;
      Alcotest.test_case "code block no output"    `Quick test_code_block_no_output;
      Alcotest.test_case "template int"            `Quick test_template_int;
      Alcotest.test_case "template float"          `Quick test_template_float;
      Alcotest.test_case "template bool"           `Quick test_template_bool;
      Alcotest.test_case "template string"         `Quick test_template_string;
      Alcotest.test_case "if true"                 `Quick test_if_true;
      Alcotest.test_case "if false"                `Quick test_if_false;
      Alcotest.test_case "if false no else"        `Quick test_if_false_no_else;
      Alcotest.test_case "if scope does not leak"  `Quick test_if_scope_does_not_leak;
    ];
  ]
