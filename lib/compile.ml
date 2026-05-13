let rec compress_markdown acc (blocks : Ptree.block_element list) =
  match blocks with
  | [] -> List.rev acc
  | { Ptree.node = Ptree.Markdown m1; ident = (loc_start, _) } ::
    { Ptree.node = Ptree.Markdown m2; ident = (_, loc_end) } :: rest ->
      let merged = { Ptree.node = Ptree.Markdown (m1 ^ m2); ident = (loc_start, loc_end) } in
      compress_markdown acc (merged :: rest)
  | b :: rest ->
      let b' = match b.Ptree.node with
        | Ptree.IfElse (cond, then_br, else_br_opt) ->
            let compressed_then = compress_markdown [] then_br in
            let compressed_else = Option.map (compress_markdown []) else_br_opt in
            { b with node = Ptree.IfElse (cond, compressed_then, compressed_else) }
        | _ -> b
      in
      compress_markdown (b' :: acc) rest

let compile source =
  let lexbuf = Lexing.from_string source in
  let raw_ast = Parser.program Lexer.token lexbuf in
  let ast = compress_markdown [] raw_ast in
  let typed_ast = Typing.typecheck ast in
  Codegen.generate typed_ast