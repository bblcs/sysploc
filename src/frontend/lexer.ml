type t = { source : char Seq.t; pos : Loc.pos }

let make source = { source = String.to_seq source; pos = { line = 0; col = 0 } }

let peek lex =
  match lex.source () with Seq.Nil -> None | Seq.Cons (c, _) -> Some c

let eat lex =
  match lex.source () with
  | Seq.Nil -> (None, lex)
  | Seq.Cons (c, next_seq) ->
      let next_pos =
        Loc.
          {
            line = (if c = '\n' then 1 else 0) + lex.pos.line;
            col = (if c = '\n' then 0 else lex.pos.col + 1);
          }
      in
      (Some c, { source = next_seq; pos = next_pos })

let advance lex = snd (eat lex)
let is_whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let rec skip_whitespace lex =
  match peek lex with
  | None -> lex
  | Some c -> if is_whitespace c then skip_whitespace (snd (eat lex)) else lex

let is_numeric = function '0' .. '9' -> true | _ -> false
let char_to_num c = int_of_char c - int_of_char '0'

let rec read_num acc lex =
  match peek lex with
  | None -> (acc, lex)
  | Some c ->
      if is_numeric c then
        let _, next_lex = eat lex in
        let num = char_to_num c in
        read_num ((acc * 10) + num) next_lex
      else (acc, lex)

let lex_num lex = read_num 0 lex

let is_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

let rec read_sym acc lex =
  match peek lex with
  | None -> (acc, lex)
  | Some c ->
      if is_id_char c then read_sym (c :: acc) (advance lex) else (acc, lex)

let lex_sym lex =
  let s_list, new_lex = read_sym [] lex in
  let s = String.of_seq (List.to_seq (List.rev s_list)) in
  let tok =
    match s with
    | "val" -> Token.Val
    | "var" -> Token.Var
    | "return" -> Token.Ret
    | _ -> Token.Id s
  in
  (tok, new_lex)

let rec skip_line_comment lex =
  match peek lex with
  | None -> lex
  | Some '\n' -> advance lex
  | Some _ -> skip_line_comment (advance lex)

type block_comment = Closed | Unclosed

let rec skip_block_comment lex =
  match peek lex with
  | None -> (Unclosed, lex)
  | Some '*' -> (
      let _, after_star = eat lex in
      match peek after_star with
      | Some '/' -> (Closed, advance after_star)
      | _ -> skip_block_comment after_star)
  | Some _ -> skip_block_comment (advance lex)

let rec next lex =
  let skipped = skip_whitespace lex in
  let yield typ ?(next_lex = advance skipped) () =
    (Token.{ kind = typ; pos = skipped.pos }, next_lex)
  in
  match peek skipped with
  | None -> (Token.{ kind = Token.EOF; pos = skipped.pos }, skipped)
  | Some '(' -> yield Token.LParen ()
  | Some ')' -> yield Token.RParen ()
  | Some '=' -> yield Token.Assign ()
  | Some ';' -> yield Token.Semi ()
  | Some '+' -> yield Token.Plus ()
  | Some '-' -> yield Token.Minus ()
  | Some '*' -> yield Token.Mult ()
  | Some '/' -> (
      let _, post_slash_lex = eat skipped in
      match peek post_slash_lex with
      | Some '/' -> next (skip_line_comment skipped)
      | Some '*' -> (
          let comment, new_lex = skip_block_comment (advance post_slash_lex) in
          match comment with
          | Unclosed ->
              yield (Token.Err "Unclosed block comment") ~next_lex:new_lex ()
          | Closed -> next new_lex)
      | _ -> yield Token.Div ())
  | Some c when is_numeric c ->
      let n, new_lex = lex_num skipped in
      yield (Token.Num n) ~next_lex:new_lex ()
  | Some c when is_id_char c ->
      let sym, new_lex = lex_sym skipped in
      yield sym ~next_lex:new_lex ()
  | Some _ -> yield (Token.Err "unknown character") ()
