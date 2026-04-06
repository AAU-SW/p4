{
  open Parser
  type mode = Markdown | Code | Template | Annotation | VerbatimBlock | VerbatimInline
  let current_mode = ref Markdown
}

let white = [' ' '\t']+
let newline = '\n' | "\r\n" | '\r'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read_markdown = parse
  | "$/"      { current_mode := Code; CODE_START }
  | "$$"      { current_mode := Annotation; ANNOTATION_START }
  | "${"      { current_mode := Template; TEMPLATE_START }
  | "```"     { current_mode := VerbatimBlock; MARKDOWN_TEXT "```" }
  | "`"       { current_mode := VerbatimInline; MARKDOWN_TEXT "`" }
  | [^ '$' '`' '\n' '\r']+ as s { MARKDOWN_TEXT s }
  | newline as nl { Lexing.new_line lexbuf; MARKDOWN_TEXT nl }
  | '$'       { MARKDOWN_TEXT "$" }
  | eof       { EOF }

and read_code = parse
  | "/$"      { current_mode := Markdown; CODE_END }
  | white     { read_code lexbuf }
  | newline   { Lexing.new_line lexbuf; read_code lexbuf }
  | "//" [^ '\n']* (newline | eof) { Lexing.new_line lexbuf; read_code lexbuf }
  | "Float"   { T_FLOAT }
  | "Int"     { T_INT }
  | "Bool"    { T_BOOL }
  | "String"  { T_STRING }
  | "Abbr"    { ABBR }
  | "if"      { IF }
  | "else"    { ELSE }
  | "true"    { BOOL_LIT true }
  | "false"   { BOOL_LIT false }
  | "=="      { EQ_OP }
  | ">="      { GTEQ_OP }
  | "<="      { LTEQ_OP }
  | ">"       { GT_OP }
  | "<"       { LT_OP }
  | "="       { ASSIGN }
  | ";"       { SEMI }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "{"       { LBRACE }
  | "}"       { IF_END }
  | '"'       { read_string (Buffer.create 16) lexbuf }
  | ['0'-'9']+ as i { INT_LIT (int_of_string i) }
  | ['0'-'9']+ '.' ['0'-'9']* as f { FLOAT_LIT (float_of_string f) }
  | ident as id { IDENT id }
  | _         { failwith ("Unexpected character in code block: " ^ Lexing.lexeme lexbuf) }

and read_template = parse
  | "}"       { current_mode := Markdown; TEMPLATE_END }
  | white     { read_template lexbuf }
  | ident as id { IDENT id }
  | _         { failwith "Invalid character inside template block" }

and read_annotation = parse
  | white     { read_annotation lexbuf }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | ident as id { IDENT id }
  | newline   { Lexing.new_line lexbuf; current_mode := Markdown; ANNOTATION_END }
  | eof       { current_mode := Markdown; ANNOTATION_END }
  | _ as c    { failwith (Printf.sprintf "Unexpected character in annotation: '%c'" c) }

and read_string buf = parse
  | '"'       { STRING_LIT (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf }
  | eof       { failwith "Unterminated string literal" }

and read_verbatim_block = parse
  | "```"     { current_mode := Markdown; MARKDOWN_TEXT "```" }
  | [^ '`' '\n' '\r']+ as s { MARKDOWN_TEXT s }
  | newline as nl { Lexing.new_line lexbuf; MARKDOWN_TEXT nl }
  | '`'       { MARKDOWN_TEXT "`" }
  | eof       { EOF }

and read_verbatim_inline = parse
  | "`"       { current_mode := Markdown; MARKDOWN_TEXT "`" }
  | [^ '`' '\n' '\r']+ as s { MARKDOWN_TEXT s }
  | newline as nl { Lexing.new_line lexbuf; MARKDOWN_TEXT nl }
  | eof       { EOF }

{
  let token lexbuf =
    match !current_mode with
    | Markdown -> read_markdown lexbuf
    | Code -> read_code lexbuf
    | Template -> read_template lexbuf
    | Annotation -> read_annotation lexbuf
    | VerbatimBlock -> read_verbatim_block lexbuf
    | VerbatimInline -> read_verbatim_inline lexbuf
}
