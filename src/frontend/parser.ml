type error = { pos : Loc.pos; msg : string }

type 'a parse_result =
  | Ok of 'a * Token.t list * error list
  | Error of error list

let eof_loc = Loc.{ line = -1; col = -1 }

type 'a parser = Parser of (Token.t list -> 'a parse_result)

let cur_pos = function [] -> eof_loc | t :: _ -> t.Token.pos

(** haskell at home: *)
let unwrap (Parser pf) inp = pf inp

let error msg =
  Parser
    (fun inp ->
      let pos = cur_pos inp in
      Error [ { pos; msg } ])

let result v = Parser (fun inp -> Ok (v, inp, []))
let zero = error "zero"

let item =
  Parser
    (function
    | [] -> Error [ { pos = eof_loc; msg = "Unexpected EOF in item." } ]
    | tok :: rem -> Ok (tok, rem, []))

let choice (Parser pf1) (Parser pf2) =
  Parser
    (fun inp ->
      match pf1 inp with
      | Ok _ as success -> success
      | Error e1 -> (
          match pf2 inp with
          | Ok _ as success -> success
          | Error e2 -> Error (e1 @ e2)))

let ( <|> ) = choice

let expected (Parser pf) msg =
  Parser
    (fun inp ->
      match pf inp with
      | Ok _ as success -> success
      | Error _ -> Error [ { pos = cur_pos inp; msg = "Expected: " ^ msg } ])

let ( <?> ) = expected

let bind (Parser pf) f =
  Parser
    (fun inp ->
      match pf inp with
      | Ok (v, rem, e1) -> (
          let (Parser fp) = f v in
          match fp rem with
          | Ok (v2, rem2, e2) -> Ok (v2, rem2, e1 @ e2)
          | Error e2 -> Error (e1 @ e2))
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
      let rec loop acc rem errs =
        match pf rem with
        | Error _ -> Ok (List.rev acc, rem, errs)
        | Ok (v, nrem, e) ->
            if rem == nrem then Ok (List.rev acc, rem, errs)
            else loop (v :: acc) nrem (errs @ e)
      in
      loop [] inp [])

and many1 p =
  let* fst = p in
  let+ rem = many p in
  fst :: rem

let recover (Parser pf) syncs sentinel =
  Parser
    (fun inp ->
      match pf inp with
      | Ok _ as ok -> ok
      | Error e ->
          let rec loop rem =
            match rem with
            | t :: nrem ->
                if List.mem t.Token.kind syncs then Ok (sentinel, rem, e)
                else loop nrem
            | [] ->
                Error [ { pos = eof_loc; msg = "Unexpected EOF on revovery" } ]
          in
          loop inp)

let opval p op =
  let* f = op in
  let+ y = p in
  (f, y)

let bracket ope p clo = tok ope *> p <* tok clo

let parens p =
  let safe = recover p [ Token.RParen; Token.EOF ] Ast.ErrorExpr in
  bracket Token.LParen safe Token.RParen

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

(* NOTE questionable*)
and parse_expr =
  Parser
    (fun inp ->
      unwrap (recover parse_addexp [ Token.Semi; Token.EOF ] Ast.ErrorExpr) inp)

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

let parse_stmt_rule =
  parse_ret_stmt <|> parse_decl_stmt <|> parse_assign_stmt <|> parse_expr_stmt
  <?> "statement"

let parse_stmt = recover parse_stmt_rule [ Token.Semi; Token.EOF ] Ast.ErrorStmt
let rec parse_program = many parse_stmt <* tok Token.EOF
