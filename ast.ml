(* Abstract Syntax of Markdowns *)
type abbreviation = string

type constant =
  | Cn of float
  | Cb of bool
  | Cs of string
  | Ca of abbreviation

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * // % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* and or *)

type expr =
  | Ec of constant
  | Eid of string
  | Ebinop of binop * expr * expr       (* binary operation *)

type stmt =
  | Sassign of string * expr            (* assignment *)
  | Sseq of stmt * stmt                 (* sequence of statements *)
  | Sif of expr * stmt * stmt           (* if-then-else *)

type doc_element =
  | RawText of string
  | Annotation of string
  | Script of stmt                      (* The §§ or §/ /§ blocks *)

type document = doc_element list
