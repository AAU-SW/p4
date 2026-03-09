{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "if", IF; "else", ELSE;
        "and", AND; "or", OR; "not", NOT;
        "true", CST (Cbool true);
        "false", CST (Cbool false);
        "Float", FLOAT; "Bool", BOOL; "String", STRING; "Abbr", ABBREVIATION;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let float_literal = digit+ '.' digit* | '.' digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* RULE 1: TEXT MODE (The default state) *)
rule next_tokens = parse
  | eof             { [EOF] }
  | white           { next_tokens lexbuf }
  | newline         { new_line lexbuf; next_tokens lexbuf }

  (* Multi-line Code Block *)
  | "§/"            { OPEN_CODE :: (read_code [] lexbuf) }

  (* Single-line Annotation *)
  (* We emit ANNOTATION, then immediately parse the rest of the line as code *)
  | "§§"            { ANNOTATION :: (read_oneline_code [] lexbuf) }

  (* Plain Text *)
  | _ as c          { [TEXT (String.make 1 c)] }


(* RULE 2: CODE MODE *)
and read_code acc = parse
  | white           { read_code acc lexbuf }
  | newline         { new_line lexbuf; read_code acc lexbuf }

  (* EXIT CODE MODE:
     When we see /§, we add CLOSE_CODE, reverse the list (since we built it backwards),
     and return the full list of tokens to the parser. *)
  | "/§"            { List.rev (CLOSE_CODE :: acc) }

  (* Keywords & Identifiers *)
  | ident as id     { read_code ((id_or_kwd id) :: acc) lexbuf }

  (* Literals *)
  | float_literal as f { read_code (CST (Cfloat (float_of_string f)) :: acc) lexbuf }
  | digit+ as i        { read_code (CST (Cfloat (float_of_string i)) :: acc) lexbuf }
  | '"'                { read_code (CST (Cstring (string lexbuf)) :: acc) lexbuf }

  (* Operators - Explicitly adding to accumulator *)
  | "+"             { read_code (PLUS :: acc) lexbuf }
  | "-"             { read_code (MINUS :: acc) lexbuf }
  | "*"             { read_code (TIMES :: acc) lexbuf }
  | "/"             { read_code (DIV :: acc) lexbuf }
  | "="             { read_code (EQUAL :: acc) lexbuf }
  | "=="            { read_code (CMP Beq :: acc) lexbuf }
  | "!="            { read_code (CMP Bneq :: acc) lexbuf }
  | "<"             { read_code (CMP Blt :: acc) lexbuf }
  | "<="            { read_code (CMP Ble :: acc) lexbuf }
  | ">"             { read_code (CMP Bgt :: acc) lexbuf }
  | ">="            { read_code (CMP Bge :: acc) lexbuf }
  | "("             { read_code (LP :: acc) lexbuf }
  | ")"             { read_code (RP :: acc) lexbuf }
  | ","             { read_code (COMMA :: acc) lexbuf }
  | ":"             { read_code (COLON :: acc) lexbuf }
  | "//" [^'\n']* { read_code acc lexbuf } (* Comments inside code block *)

  | eof             { raise (Lexing_error "Code block not closed before EOF") }
  | _ as c          { raise (Lexing_error ("Illegal character in code block: " ^ String.make 1 c)) }

(* RULE 3: STRING PARSING *)
and string = parse
  | '"'             { let s = Buffer.contents string_buffer in
                      Buffer.reset string_buffer; s }
  | "\\n"           { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\\""          { Buffer.add_char string_buffer '"'; string lexbuf }
  | _ as c          { Buffer.add_char string_buffer c; string lexbuf }
  | eof             { raise (Lexing_error "unterminated string") }

and read_oneline_code acc = parse
  | white               { read_oneline_code acc lexbuf }

  (* EXIT CONDITION: Newline *)
  (* We consume the newline, update line count, and return the list. *)
  | newline             { new_line lexbuf; List.rev acc }

  (* We DO NOT allow multi-line comments or blocks inside here *)

  (* Keywords & Identifiers *)
  | ident as id         { read_oneline_code ((id_or_kwd id) :: acc) lexbuf }

  (* Literals *)
  | float_literal as f  { read_oneline_code (CST (Cfloat (float_of_string f)) :: acc) lexbuf }
  | digit+ as i         { read_oneline_code (CST (Cfloat (float_of_string i)) :: acc) lexbuf }
  | '"'                 { read_oneline_code (CST (Cstring (string lexbuf)) :: acc) lexbuf }

  (* Operators - Explicitly adding to accumulator *)
  | "+"                 { read_oneline_code (PLUS :: acc) lexbuf }
  | "-"                 { read_oneline_code (MINUS :: acc) lexbuf }
  | "*"                 { read_oneline_code (TIMES :: acc) lexbuf }
  | "/"                 { read_oneline_code (DIV :: acc) lexbuf }
  | "="                 { read_oneline_code (EQUAL :: acc) lexbuf }
  | "=="                { read_oneline_code (CMP Beq :: acc) lexbuf }
  | "!="                { read_oneline_code (CMP Bneq :: acc) lexbuf }
  | "<"                 { read_oneline_code (CMP Blt :: acc) lexbuf }
  | "<="                { read_oneline_code (CMP Ble :: acc) lexbuf }
  | ">"                 { read_oneline_code (CMP Bgt :: acc) lexbuf }
  | ">="                { read_oneline_code (CMP Bge :: acc) lexbuf }
  | "("                 { read_oneline_code (LP :: acc) lexbuf }
  | ")"                 { read_oneline_code (RP :: acc) lexbuf }
  | ","                 { read_oneline_code (COMMA :: acc) lexbuf }
  | ":"                 { read_oneline_code (COLON :: acc) lexbuf }
  | "//" [^'\n']*       { read_oneline_code acc lexbuf } (* Comments inside code block *)

| eof                   { List.rev acc } (* Allow file to end on an annotation *)
| _ as c                { raise (Lexing_error ("Illegal character in annotation: " ^ String.make 1 c)) }

{
  (* Helper for the parser to pop tokens one by one *)
  let next_token =
    let tokens = Queue.create () in
    fun lb ->
      if Queue.is_empty tokens then begin
        let l = next_tokens lb in
        List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
