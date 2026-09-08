open Frontend

let rec dump_tokens lexer =
  let tok, next_lexer = Lexer.next lexer in
  print_endline (Token.show_token tok);
  match tok.kind with Token.EOF -> () | _ -> dump_tokens next_lexer

let () =
  let src = In_channel.input_all stdin in
  print_endline src;
  dump_tokens (Lexer.make src)
