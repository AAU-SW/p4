(* Abstract Syntax of Markdowns *)

type location = Lexing.position * Lexing.position
type ident = { loc: location; id: string }

type abbreviation = string

type constant =
  | Cfloat of float
  | Cbool of bool
  | Cstring of string
  | Cabbr of abbreviation

type unop =
  | Unot

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                           (* and or *)

type expr =
  | Ec of constant
  | Eident of ident
  | Ebinop of binop * expr * expr        (* binary operation *)
  | Eunop of unop * expr

type stmt =
  | Sassign of ident * expr              (* assignment *)
  | Sseq of stmt * stmt                  (* sequence of statements *)
  | Sif of expr * stmt * stmt            (* if-then-else *)
  | Snop                                 (* empty branch *)

type doc_element =
  | RawText of string
  | Annotation of expr                   
  | Script of stmt                       (* The §§ or §/ /§ blocks *)

type document = doc_element list