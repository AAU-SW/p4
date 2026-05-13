open Markdowns_lib

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

let load_fixture name = 
  read_file (project_root ^ "/test/integration/" ^ name)

let test_variable_template () =
  let input    = load_fixture "variable_template/input.mds" in
  let expected = String.trim (load_fixture "variable_template/expected.md") in
  let actual   = String.trim (Compile.compile input) in
  Alcotest.(check string) "variable renders in template" expected actual
  

let test_conditional_template () =
  let input    = load_fixture "conditional_template/input.mds" in
  let expected = String.trim (load_fixture "conditional_template/expected.md") in
  let actual   = String.trim (Compile.compile input) in
  Alcotest.(check string) "conditional renders in template" expected actual  

let () =
  Alcotest.run "Integration Tests" [
    "variables", [
      Alcotest.test_case "variable template rendering" `Quick test_variable_template;
    ];
    "conditionals", [
      Alcotest.test_case "conditional template rendering" `Quick test_conditional_template;
     ];
  ]