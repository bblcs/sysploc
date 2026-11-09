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

let get_pos =
  Parser
    (function [] -> Ok (eof_loc, [], []) | t :: _ as inp -> Ok (t.pos, inp, []))

let with_pos p =
  let* pos = get_pos in
  let+ node = p in
  Ast.{ node; pos }

let sat p =
  Parser
    (function
    | [] -> Error [ { pos = eof_loc; msg = "Unexpected EOF" } ]
    | tok :: rem ->
        if p tok then Ok (tok, rem, [])
        else Error [ { pos = tok.pos; msg = "zero" } ])

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

let recover (Parser pf) syncs sentinel_gen =
  Parser
    (fun inp ->
      match pf inp with
      | Ok _ as ok -> ok
      | Error e ->
          let err_pos =
            List.fold_left
              (fun max_pos err ->
                if err.pos > max_pos then err.pos else max_pos)
              (cur_pos inp) e
          in
          let rec loop rem =
            match rem with
            | t :: nrem ->
                if List.mem t.Token.kind syncs then
                  Ok (sentinel_gen err_pos, rem, e)
                else loop nrem
            | [] ->
                Error [ { pos = eof_loc; msg = "Unexpected EOF on recovery" } ]
          in
          loop inp)

let err_expr pos = Ast.{ node = Ast.ErrorExpr; pos }
let err_stmt pos = Ast.{ node = Ast.ErrorStmt; pos }

let opval p op =
  let* f = op in
  let+ y = p in
  (f, y)

let bracket ope p clo = tok ope *> p <* tok clo

let parens p =
  let safe = recover p [ Token.RParen; Token.EOF ] err_expr in
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
  Parser
    (function
    | [] -> Error [ { pos = eof_loc; msg = "Unexpected EOF" } ]
    | tok :: rem -> (
        match f tok.kind with
        | Some v -> Ok (v, rem, [])
        | None -> Error [ { pos = tok.pos; msg = err } ]))

let parse_name =
  parse_valued (function Token.Id s -> Some s | _ -> None) "Expected Id"

let parse_num =
  parse_valued (function Token.Num n -> Some n | _ -> None) "Expected Num"

let parse_bool =
  parse_valued
    (function Token.True -> Some true | Token.False -> Some false | _ -> None)
    "Expected Bool"

let parse_id =
  let+ name = parse_name in
  Ast.Id name

let parse_intlit =
  let+ num = parse_num in
  Ast.IntLit num

let parse_boollit =
  let+ bool = parse_bool in
  Ast.BoolLit bool

let binop t ast_op =
  let* op_tok = tok t in
  result (fun l r -> Ast.{ node = Ast.BinOp (l, ast_op, r); pos = op_tok.pos })

let parse_additive = binop Token.Plus Ast.Add <|> binop Token.Minus Ast.Sub
let parse_multiplicative = binop Token.Mult Ast.Mul <|> binop Token.Div Ast.Div

let parse_relational =
  binop Token.Lt Ast.Lt <|> binop Token.Gt Ast.Gt <|> binop Token.Le Ast.Le
  <|> binop Token.Ge Ast.Ge

let parse_eq = binop Token.Eq Ast.Eq <|> binop Token.Ne Ast.Ne
let parse_and = binop Token.And Ast.And
let parse_or = binop Token.And Ast.And

let rec parse_prim =
  Parser
    (fun inp ->
      unwrap
        (with_pos (parse_intlit <|> parse_id <|> parse_boollit)
        <|> parens parse_expr)
        inp)

and parse_unexp =
  Parser
    (fun inp ->
      unwrap
        (parse_prim
        <|> let* op = tok Token.Minus <|> tok Token.Not in
            let+ prim = parse_unexp in
            if op.kind = Token.Minus then
              Ast.{ node = Ast.UnaryMinus prim; pos = op.pos }
            else Ast.{ node = Ast.UnaryNot prim; pos = op.pos })
        inp)

and parse_multexp =
  Parser (fun inp -> unwrap (chainl1 parse_unexp parse_multiplicative) inp)

and parse_addexp =
  Parser (fun inp -> unwrap (chainl1 parse_multexp parse_additive) inp)

and parse_relexp =
  Parser (fun inp -> unwrap (chainl1 parse_addexp parse_relational) inp)

and parse_eqexp = Parser (fun inp -> unwrap (chainl1 parse_relexp parse_eq) inp)

and parse_andexp =
  Parser (fun inp -> unwrap (chainl1 parse_eqexp parse_and) inp)

and parse_orexp = Parser (fun inp -> unwrap (chainl1 parse_andexp parse_or) inp)

(* NOTE questionable*)
and parse_expr =
  Parser
    (fun inp ->
      unwrap
        (recover parse_orexp
           [ Token.Semi; Token.RParen; Token.RBrace; Token.EOF ]
           err_expr)
        inp)

let parse_expr_stmt =
  let+ expr = parse_expr <* tok Token.Semi in
  Ast.Expr expr

let parse_assign_stmt =
  let* name = with_pos parse_name in
  let* eq = tok Token.Assign in
  let+ expr = parse_expr <* tok Token.Semi in
  Ast.{ node = Ast.Assignment (name, expr); pos = eq.pos }

let parse_decltype =
  tok Token.Var *> result Ast.Mut <|> tok Token.Val *> result Ast.Const

let parse_decl_stmt =
  let* decltype = parse_decltype in
  let* name = with_pos parse_name in
  let* expr =
    recover (tok Token.Assign *> parse_expr) [ Token.Semi; Token.EOF ] err_expr
  in
  let+ _ = tok Token.Semi in
  Ast.Decl (name, expr, decltype)

let parse_ret_stmt =
  let+ expr = tok Token.Ret *> parse_expr <* tok Token.Semi in
  Ast.Ret expr

let parse_break_stmt =
  let+ _ = tok Token.Break <* tok Token.Semi in
  Ast.Break

let parse_continue_stmt =
  let+ _ = tok Token.Cont <* tok Token.Semi in
  Ast.Continue

let rec parse_if_stmt =
  Parser
    (fun inp ->
      unwrap
        (let* _ = tok Token.If in
         let* cond = parens parse_expr in
         let* then_stmt = parse_stmt in
         let+ else_stmt =
           (let* _ = tok Token.Else in
            let+ s = parse_stmt in
            Some s)
           <|> result None
         in
         Ast.If (cond, then_stmt, else_stmt))
        inp)

and parse_while_stmt =
  Parser
    (fun inp ->
      unwrap
        (let* _ = tok Token.While in
         let* cond = parens parse_expr in
         let+ body = parse_stmt in
         Ast.While (cond, body))
        inp)

and parse_block =
  Parser
    (fun inp ->
      unwrap
        (let+ stmts = bracket Token.LBrace (many parse_stmt) Token.RBrace in
         Ast.Block stmts)
        inp)

and parse_stmt_rule =
  Parser
    (fun inp ->
      unwrap
        (parse_assign_stmt
        <|> with_pos
              (parse_ret_stmt <|> parse_decl_stmt <|> parse_if_stmt
             <|> parse_while_stmt <|> parse_break_stmt <|> parse_continue_stmt
             <|> parse_block <|> parse_expr_stmt <?> "statement"))
        inp)

and parse_stmt =
  Parser
    (fun inp ->
      unwrap
        (recover parse_stmt_rule
           [ Token.Semi; Token.RBrace; Token.EOF ]
           err_stmt)
        inp)

let rec parse_program = many parse_stmt <* tok Token.EOF
