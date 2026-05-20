open Alcotest
open Markdowns_lib
open Ptree

(* Constructs a Ptree program directly without going through the lexer or
   parser, so these are true unit tests of the typechecker in isolation. *)

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)
let mk node = { node; ident = dummy_loc }

let typecheck ast = Typing.typecheck ast

let check_passes msg ast =
  match typecheck ast with
  | _ -> ()
  | exception Typing.TypeError (e, _) -> fail (msg ^ ": unexpected TypeError: " ^ e)

let check_fails msg ast =
  match typecheck ast with
  | _ -> fail (msg ^ ": expected TypeError but typechecking passed")
  | exception Typing.TypeError _ -> ()

(* --- Valid programs --- *)

(* A declared variable can be used in a template without error. *)
let test_var_decl_and_template () =
  check_passes "var decl and template" [
    mk (CodeBlock [ mk (SVarDecl (TyInt, "x", mk (EInt 42))) ]);
    mk (Template "x")
  ]

(* An abbreviation declared with Abbr can be referenced in an annotation. *)
let test_abbr_decl_and_annotation () =
  check_passes "abbr decl and annotation" [
    mk (CodeBlock [ mk (SAbbrDecl ("API", "Application Programming Interface")) ]);
    mk (Annotation ("Req", ["API"]))
  ]

(* Comparing two values of the same type is valid. *)
let test_binop_same_type () =
  check_passes "binop same type" [
    mk (CodeBlock [
      mk (SVarDecl (TyInt, "x", mk (EInt 1)));
      mk (SVarDecl (TyBool, "b", mk (EBinop (Eq, mk (EInt 1), mk (EInt 2)))))
    ])
  ]

(* An if condition that is a Bool is valid. *)
let test_if_bool_condition () =
  check_passes "if bool condition" [
    mk (IfElse (mk (EBool true), [ mk (Markdown "yes") ], None))
  ]

(* Relational operators on Int values are valid. *)
let test_relational_on_int () =
  check_passes "relational on int" [
    mk (CodeBlock [
      mk (SVarDecl (TyBool, "b", mk (EBinop (Gt, mk (EInt 2), mk (EInt 1)))))
    ])
  ]

(* --- Invalid programs --- *)

(* Using a variable that has never been declared is a type error. *)
let test_unbound_variable () =
  check_fails "unbound variable" [
    mk (Template "x")
  ]

(* Declaring the same variable twice is a type error (variables are immutable). *)
let test_redeclare_variable () =
  check_fails "redeclare variable" [
    mk (CodeBlock [
      mk (SVarDecl (TyInt, "x", mk (EInt 1)));
      mk (SVarDecl (TyInt, "x", mk (EInt 2)))
    ])
  ]

(* Assigning a value whose type differs from the declared type is a type error. *)
let test_type_mismatch_assignment () =
  check_fails "type mismatch on assignment" [
    mk (CodeBlock [
      mk (SVarDecl (TyInt, "x", mk (EBool true)))
    ])
  ]

(* Comparing values of two different types is a type error. *)
let test_binop_type_mismatch () =
  check_fails "binop type mismatch" [
    mk (CodeBlock [
      mk (SVarDecl (TyBool, "b", mk (EBinop (Eq, mk (EInt 1), mk (EBool true)))))
    ])
  ]

(* Relational operators on non-numeric types (String, Bool) are a type error. *)
let test_relational_on_string () =
  check_fails "relational op on string" [
    mk (CodeBlock [
      mk (SVarDecl (TyBool, "b", mk (EBinop (Gt, mk (EString "a"), mk (EString "b")))))
    ])
  ]

let test_relational_on_bool () =
  check_fails "relational op on bool" [
    mk (CodeBlock [
      mk (SVarDecl (TyBool, "b", mk (EBinop (Lt, mk (EBool true), mk (EBool false)))))
    ])
  ]

(* An if condition that is not a Bool is a type error. *)
let test_if_non_bool_condition () =
  check_fails "if non-bool condition" [
    mk (IfElse (mk (EInt 1), [ mk (Markdown "yes") ], None))
  ]

(* Using an abbreviation in Req before declaring it with Abbr is a type error. *)
let test_annotation_undeclared_abbr () =
  check_fails "annotation with undeclared abbr" [
    mk (Annotation ("Req", ["API"]))
  ]

(* Using a variable in a template before it has been declared is a type error. *)
let test_template_unbound_variable () =
  check_fails "template with unbound variable" [
    mk (Template "x")
  ]

let () =
  Alcotest.run "Typing Tests" [
    "valid", [
      Alcotest.test_case "var decl and template"    `Quick test_var_decl_and_template;
      Alcotest.test_case "abbr decl and annotation" `Quick test_abbr_decl_and_annotation;
      Alcotest.test_case "binop same type"          `Quick test_binop_same_type;
      Alcotest.test_case "if bool condition"        `Quick test_if_bool_condition;
      Alcotest.test_case "relational on int"        `Quick test_relational_on_int;
    ];
    "invalid", [
      Alcotest.test_case "unbound variable"           `Quick test_unbound_variable;
      Alcotest.test_case "redeclare variable"         `Quick test_redeclare_variable;
      Alcotest.test_case "type mismatch assignment"   `Quick test_type_mismatch_assignment;
      Alcotest.test_case "binop type mismatch"        `Quick test_binop_type_mismatch;
      Alcotest.test_case "relational on string"       `Quick test_relational_on_string;
      Alcotest.test_case "relational on bool"         `Quick test_relational_on_bool;
      Alcotest.test_case "if non-bool condition"      `Quick test_if_non_bool_condition;
      Alcotest.test_case "annotation undeclared abbr" `Quick test_annotation_undeclared_abbr;
      Alcotest.test_case "template unbound variable"  `Quick test_template_unbound_variable;
    ];
  ]
