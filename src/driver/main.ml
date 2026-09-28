open Frontend

let print_error e =
  Format.eprintf "Error at %d:%d: %s\n" (e.Parser.pos.line + 1) (e.pos.col + 1)
    e.msg

let rec separate_tokens tokens =
  let rec loop valid errs = function
    | [] -> (List.rev valid, List.rev errs)
    | t :: ts -> (
        match t.Token.kind with
        | Token.Err msg -> loop valid (Parser.{ pos = t.pos; msg } :: errs) ts
        | Token.ErrInvalidChar c ->
            loop valid
              (Parser.
                 {
                   pos = t.pos;
                   msg = Format.sprintf "Invalid character '%c'" c;
                 }
              :: errs)
              ts
        | _ -> loop (t :: valid) errs ts)
  in
  loop [] [] tokens

let rec get_tokens lexer =
  let rec collect lex acc =
    let tok, next_lex = Lexer.next lex in
    let acc = tok :: acc in
    if tok.kind = Token.EOF then List.rev acc else collect next_lex acc
  in
  collect lexer []

let dump_tokens_json tokens =
  `List (List.map Token.to_yojson tokens) |> Yojson.Basic.pretty_to_string

let dump_to_file filename data =
  Out_channel.with_open_text filename (fun oc ->
      Out_channel.output_string oc data)

let usage_msg =
  "usage: sysploc [-t <dump tokens file> | -a <dump ast file> | -v] <source \
   file>"

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
  let valid_tokens, lex_errors = separate_tokens all_tokens in

  if !dump_ast_file = "" then begin
    if lex_errors <> [] then begin
      List.iter print_error lex_errors;
      exit 1
    end;
    exit 0
  end;

  let ast_opt, rem_tok, parse_errs =
    match Parser.unwrap Parser.parse_program valid_tokens with
    | Ok (ast, rem, errs) -> (Some ast, rem, errs)
    | Error errs -> (None, [], errs)
  in
  let all_errs = lex_errors @ parse_errs in
  if !verbose then begin
    print_endline "parsing:";
    (match ast_opt with
    | Some ast ->
        print_endline
          (if all_errs = [] then "parsed fine" else "parsed with errors");
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
