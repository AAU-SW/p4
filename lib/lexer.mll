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
        "Float", TFLOAT; "Bool", TBOOL; "String", TSTRING; "Abbr", TABBR; (* Types added *)
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_' | '-')*
let float_literal = digit+ '.' digit* | '.' digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule next_tokens = parse
  | eof             { [EOF] }
  | white as w      { [TEXT w] }
  | newline as n    { new_line lexbuf; [TEXT n] }
  | "§/"            { OPEN_CODE :: (read_code [] lexbuf) }
  | "§§"            { ANNOTATION :: (read_oneline_code [] lexbuf) }
  | _ as c          { [TEXT (String.make 1 c)] }

and read_code acc = parse
  | white           { read_code acc lexbuf }
  | newline         { new_line lexbuf; read_code acc lexbuf }
  | "/§"            { List.rev (CLOSE_CODE :: acc) }
  | ident as id     { read_code ((id_or_kwd id) :: acc) lexbuf }
  | float_literal as f { read_code (CST (Cfloat (float_of_string f)) :: acc) lexbuf }
  | digit+ as i        { read_code (CST (Cfloat (float_of_string i)) :: acc) lexbuf }
  | '"'                { Buffer.reset string_buffer; read_code (CST (Cstring (string lexbuf)) :: acc) lexbuf }
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
  | "{"             { read_code (LBRACE :: acc) lexbuf } (* Added Braces *)
  | "}"             { read_code (RBRACE :: acc) lexbuf }
  | ";"             { read_code (SEMI :: acc) lexbuf }   (* Added Semicolons *)
  | ","             { read_code (COMMA :: acc) lexbuf }
  | "//" [^'\n']* { read_code acc lexbuf }
  | eof             { raise (Lexing_error "Code block not closed before EOF") }
  | _ as c          { raise (Lexing_error ("Illegal character in code block: " ^ String.make 1 c)) }

and string = parse
  | '"'             { Buffer.contents string_buffer }
  | "\\n"           { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\\""          { Buffer.add_char string_buffer '"'; string lexbuf }
  | _ as c          { Buffer.add_char string_buffer c; string lexbuf }
  | eof             { raise (Lexing_error "unterminated string") }

and read_oneline_code acc = parse
  | white               { read_oneline_code acc lexbuf }
  | newline             { new_line lexbuf; List.rev acc }
  | ident as id         { read_oneline_code ((id_or_kwd id) :: acc) lexbuf }
  | float_literal as f  { read_oneline_code (CST (Cfloat (float_of_string f)) :: acc) lexbuf }
  | digit+ as i         { read_oneline_code (CST (Cfloat (float_of_string i)) :: acc) lexbuf }
  | '"'                 { Buffer.reset string_buffer; read_oneline_code (CST (Cstring (string lexbuf)) :: acc) lexbuf }
  | "="                 { read_oneline_code (EQUAL :: acc) lexbuf }
  | "("                 { read_oneline_code (LP :: acc) lexbuf }
  | ")"                 { read_oneline_code (RP :: acc) lexbuf }
  | eof                 { List.rev (EOF :: acc) }
  | _                   { read_oneline_code acc lexbuf } (* Ignore other chars in annotations for brevity *)

{
  let next_token =
    let tokens = Queue.create () in
    fun lb ->
      if Queue.is_empty tokens then begin
        let l = next_tokens lb in
        List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
