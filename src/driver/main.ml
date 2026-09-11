open Frontend

let there_were_errors = ref false

let rec get_tokens lexer =
  let rec collect lex acc =
    let tok, next_lex = Lexer.next lex in
    if Token.is_error tok then begin
      there_were_errors := true
    end;
    if tok.kind = Token.EOF then List.rev (tok :: acc)
    else collect next_lex (tok :: acc)
  in
  collect lexer []

let dump_tokens_json lexer =
  let tokens = get_tokens lexer in
  let json = `List (List.map Token.to_yojson tokens) in
  Yojson.Basic.pretty_to_string json

let dump_to_file filename data =
  Out_channel.with_open_text filename (fun oc ->
      Out_channel.output_string oc data)

let usage_msg =
  "usage: sysploc [-t <dump tokens file> | -a <dump ast file> -v] <source file>"

let dump_tokens_file = ref ""
let dump_ast_file = ref ""
let verbose = ref false
let source_file = ref ""
let anon_fun source = source_file := source

let speclist =
  [
    ( "-t",
      Arg.Set_string dump_tokens_file,
      "Set a file to dump tokens as a json to" );
    ("-a", Arg.Set_string dump_ast_file, "Set a file to dump ast as a json to");
    ("-v", Arg.Set verbose, "Duplicate json dumps and input into stdout");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !source_file = "" then begin
    print_endline usage_msg;
    exit 1
  end;
  let src =
    In_channel.with_open_text !source_file (fun ic -> In_channel.input_all ic)
  in
  let lexer = Lexer.make src in
  if !verbose then begin
    print_endline "source:";
    print_endline src;
    print_endline "lexing:";
    print_endline (dump_tokens_json lexer)
  end;
  if !dump_tokens_file <> "" then begin
    dump_to_file !dump_tokens_file (dump_tokens_json lexer)
  end;
  if !dump_ast_file <> "" then begin
    let tokens = get_tokens lexer in
    let res = Parser.unwrap Parser.parse_program tokens in
    match res with
    | Ok (ast, []) ->
        print_endline "parsed fine";
        List.iter (fun stmt -> print_endline (Ast.show_statement stmt)) ast
    | Ok (ast, rem) ->
        print_endline "parsed with a remainder";
        List.iter (fun tok -> print_endline (Token.show tok)) rem
    | Error s -> failwith s
  end;
  exit (match !there_were_errors with true -> 1 | false -> 0)
