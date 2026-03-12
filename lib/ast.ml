(* Abstract Syntax of Markdowns *)

type location = Lexing.position * Lexing.position
type ident = { loc: location; id: string }

(* Added strong types *)
type typ =
  | TFloat
  | TBool
  | TString
  | TAbbr

type constant =
  | Cfloat of float
  | Cbool of bool
  | Cstring of string

type unop = Unot

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq | Bneq | Blt | Ble | Bgt | Bge
  | Band | Bor

type expr =
  | Ec of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecall of ident * expr list        (* Added for function calls e.g., today() *)

type stmt =
  | Svardef of typ * ident * expr     (* e.g., Float f = 0.0; (Immutable assignment) *)

type doc_element =
  | RawText of string
  | Annotation of ident * ident       (* e.g., §§ Req(API) *)
  | Script of stmt list               (* Regular code block *)
  | IfBlock of expr * document * document (* if(cond) { doc } else { doc } *)

and document = doc_element list
