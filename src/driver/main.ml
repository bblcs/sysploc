open Frontend

let there_were_errors = ref false

let print_error e =
  Format.printf "Error at %d:%d: %s\n" (e.Parser.pos.line + 1) (e.pos.col + 1)
    e.msg

let rec extract_lex_errors tokens =
  List.filter_map
    (fun t ->
      match t.Token.kind with
      | Token.Err msg -> Some Parser.{ pos = t.pos; msg }
      | Token.ErrInvalidChar c ->
          Some
            Parser.
              { pos = t.pos; msg = Format.sprintf "Invalid character '%c'" c }
      | _ -> None)
    tokens

let rec extract_lex_nonerrors tokens =
  List.filter (fun t -> not (Token.is_error t)) tokens

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

let dump_tokens_json tokens =
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
  let all_tokens = get_tokens lexer in
  if !verbose then begin
    print_endline "source:";
    print_endline src;
    print_endline "lexing:";
    List.iter (fun t -> print_string (Token.show t ^ " ")) all_tokens;
    print_endline ""
  end;
  if !dump_tokens_file <> "" then begin
    dump_to_file !dump_tokens_file (dump_tokens_json all_tokens)
  end;
  let lex_errors = extract_lex_errors all_tokens in
  let valid_tokens = extract_lex_nonerrors all_tokens in

  let ast_opt, rem_tok, parse_errs =
    match Parser.unwrap Parser.parse_program valid_tokens with
    | Ok (ast, rem, errs) -> (Some ast, rem, errs)
    | Error errs -> (None, [], errs)
  in
  let all_errs = lex_errors @ parse_errs in
  if !dump_ast_file <> "" || !verbose then begin
    (match ast_opt with
    | Some ast ->
        if all_errs = [] then print_endline "parsed fine"
        else print_endline "parsed with errors";
        Ast.print_program ast
    | None -> print_endline "ultimate parser fail");
    if rem_tok <> [] then begin
      print_endline "parsed with a remainder:";
      List.iter (fun tok -> print_endline (Token.show tok)) rem_tok
    end
  end;
  if all_errs <> [] then begin
    List.iter print_error all_errs;
    exit 1
  end
  else begin
    exit 0
  end
