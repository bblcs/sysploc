type 'a parser =
  | Parser of (Token.t list -> ('a * Token.t list, string) result)

(** haskell at home: *)
let unwrap (Parser pf) inp = pf inp

let error s = Parser (fun _ -> Error s)
let result v = Parser (fun inp -> Ok (v, inp))
let zero = Parser (fun _ -> Error "zero")

let item =
  Parser (function [] -> Error "item on empty" | tok :: rem -> Ok (tok, rem))

let choice (Parser pf1) (Parser pf2) =
  Parser
    (fun inp ->
      match pf1 inp with
      | Ok _ as success -> success
      | Error e1 -> (
          match pf2 inp with
          | Ok _ as success -> success
          | Error e2 -> Error (e1 ^ " or " ^ e2)))

let ( <|> ) = choice

let bind (Parser pf) f =
  Parser
    (fun inp ->
      match pf inp with
      | Ok (v, rem) ->
          let (Parser fp) = f v in
          fp rem
      | Error _ as e -> e)

let ( >>= ) = bind
let ( let* ) = bind

let map p f =
  let* x = p in
  result (f x)

let ( >>| ) = map
let ( let+ ) = map
let ( *> ) a b = a >>= fun _ -> b

let ( <* ) a b =
  a >>= fun x ->
  b >>| fun _ -> x

let sat p =
  let* x = item in
  if p x then result x else zero

let tok kind = sat (fun cur -> cur.kind = kind)

let rec many (Parser pf) =
  Parser
    (fun inp ->
      let rec loop acc rem =
        match pf rem with
        | Error _ -> Ok (List.rev acc, rem)
        | Ok (v, nrem) -> loop (v :: acc) nrem
      in
      loop [] inp)

and many1 p =
  let* fst = p in
  let+ rem = many p in
  fst :: rem

let opval p op =
  let* f = op in
  let+ y = p in
  (f, y)

let bracket ope p clo = tok ope *> p <* tok clo
let parens p = bracket Token.LParen p Token.RParen

let chainl1 p op =
  let rec rest x =
    (let* f = op in
     let* y = p in
     rest (f x y))
    <|> result x
  in
  p >>= rest

let parse_valued f err =
  let* t = item in
  match f t.kind with Some v -> result v | None -> error err

let parse_name =
  parse_valued (function Token.Id s -> Some s | _ -> None) "Expected Id"

let parse_num =
  parse_valued (function Token.Num n -> Some n | _ -> None) "Expected Num"

let parse_id =
  let+ name = parse_name in
  Ast.Id name

let parse_intlit =
  let+ num = parse_num in
  Ast.IntLit num

let binop t ast_op = tok t *> result (fun l r -> Ast.BinOp (l, ast_op, r))
let parse_additive = binop Token.Plus Ast.Add <|> binop Token.Minus Ast.Sub
let parse_multiplicative = binop Token.Mult Ast.Mul <|> binop Token.Div Ast.Div

let rec parse_prim =
  Parser
    (fun inp -> unwrap (parse_intlit <|> parse_id <|> parens parse_expr) inp)

and parse_unexp =
  Parser
    (fun inp ->
      unwrap
        (parse_prim
        <|>
        let+ prim = tok Token.Minus *> parse_prim in
        Ast.UnaryMinus prim)
        inp)

and parse_multexp =
  Parser (fun inp -> unwrap (chainl1 parse_unexp parse_multiplicative) inp)

and parse_addexp =
  Parser (fun inp -> unwrap (chainl1 parse_multexp parse_additive) inp)

and parse_expr = Parser (fun inp -> unwrap parse_addexp inp)

let parse_expr_stmt =
  let+ expr = parse_expr <* tok Token.Semi in
  Ast.Expr expr

let parse_assign_stmt =
  let* name = parse_name <* tok Token.Assign in
  let+ expr = parse_expr <* tok Token.Semi in
  Ast.Assignment (name, expr)

let parse_decltype =
  tok Token.Var *> result Ast.Mut <|> tok Token.Val *> result Ast.Const

let parse_decl_stmt =
  let* decltype = parse_decltype in
  let* name = parse_name <* tok Token.Assign in
  let+ expr = parse_expr <* tok Token.Semi in
  Ast.Decl (name, expr, decltype)

let parse_ret_stmt =
  let+ expr = tok Token.Ret *> parse_expr <* tok Token.Semi in
  Ast.Ret expr

let parse_stmt =
  parse_ret_stmt <|> parse_decl_stmt <|> parse_assign_stmt <|> parse_expr_stmt

let rec parse_program = many parse_stmt <* tok Token.EOF
