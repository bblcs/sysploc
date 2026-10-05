type 'a ast_node = { node : 'a; pos : Loc.pos }
type ident = string ast_node
type binop = Add | Sub | Mul | Div

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

type expr_node =
  | IntLit of int64
  | Id of string
  | UnaryMinus of expr
  | BinOp of expr * binop * expr
  | ErrorExpr

and expr = expr_node ast_node

type decltype = Const | Mut [@@deriving show]

let string_of_decltype = function Const -> "const" | Mut -> "mut"

type statement_node =
  | Ret of expr
  | Decl of ident * expr * decltype
  | Assignment of ident * expr
  | Expr of expr
  | ErrorStmt

and statement = statement_node ast_node

type program = statement list

let rec repeat s n = if n = 0 then s else s ^ repeat s (n - 1)

let rec str_expr (expr : expr) ident =
  let space = repeat "  " ident in
  match expr.node with
  | IntLit n -> Format.sprintf "%sIntLit(%Ld)\n" space n
  | Id s -> Format.sprintf "%sId(%s)\n" space s
  | UnaryMinus e ->
      Format.sprintf "%sUnaryMinus of\n%s" space (str_expr e (ident + 1))
  | BinOp (lhs, op, rhs) ->
      Format.sprintf "%sBinOp %s of\n%s%s" space (string_of_binop op)
        (str_expr lhs (ident + 1))
        (str_expr rhs (ident + 1))
  | ErrorExpr -> Format.sprintf "%sErrorExpr\n" space

let str_stmt stmt ident =
  let space = repeat "  " ident in
  match stmt.node with
  | Ret e -> Format.sprintf "%sRet of\n%s" space (str_expr e (ident + 1))
  | Decl (name, e, typ) ->
      Format.sprintf "%sDecl %s %s of\n%s" space (string_of_decltype typ)
        name.node
        (str_expr e (ident + 1))
  | Assignment (name, e) ->
      Format.sprintf "%sAssignment of %s of\n%s" space name.node
        (str_expr e (ident + 1))
  | Expr e ->
      Format.sprintf "%sExprStatement of\n%s" space (str_expr e (ident + 1))
  | ErrorStmt -> Format.sprintf "%sErrorStmt\n" space

let print_program prog =
  print_endline "Program of";
  List.iter (fun stmt -> print_string (str_stmt stmt 0)) prog

let json_node pos kind elems extras =
  `Assoc
    (("line", `Int (pos.Loc.line + 1))
     :: ("column", `Int (pos.Loc.col + 1))
     :: ("kind", `String kind)
     :: extras
    @ [ ("elems", `List elems) ])

let ident_to_yojson id =
  json_node id.pos "Ident" [] [ ("value", `String id.node) ]

let rec expr_to_yojson expr =
  match expr.node with
  | IntLit n ->
      json_node expr.pos "IntLiteral" [] [ ("value", `Int (Int64.to_int n)) ]
  | Id s -> json_node expr.pos "Ident" [] [ ("value", `String s) ]
  | UnaryMinus e -> json_node expr.pos "Unary" [ expr_to_yojson e ] []
  | BinOp (lhs, op, rhs) ->
      json_node expr.pos "BinOp"
        [ expr_to_yojson lhs; expr_to_yojson rhs ]
        [ ("value", `String (string_of_binop op)) ]
  | ErrorExpr -> json_node expr.pos "Error" [] []

let stmt_to_yojson stmt =
  match stmt.node with
  | Ret e -> json_node stmt.pos "Return" [ expr_to_yojson e ] []
  | Decl (id, e, dt) ->
      let mut = match dt with Mut -> "var" | Const -> "val" in
      json_node stmt.pos "Declare"
        [ ident_to_yojson id; expr_to_yojson e ]
        [ ("mut", `String mut) ]
  | Assignment (id, e) ->
      json_node stmt.pos "Assign" [ ident_to_yojson id; expr_to_yojson e ] []
  | Expr e -> json_node stmt.pos "Expr" [ expr_to_yojson e ] []
  | ErrorStmt -> json_node stmt.pos "Error" [] []

let program_to_yojson prog =
  let stmts_json = List.map stmt_to_yojson prog in
  let pos =
    match prog with
    | [] -> Loc.{ line = 1; col = 1 }
    | start :: _ -> Loc.{ line = start.pos.line + 1; col = start.pos.col + 1 }
  in
  json_node pos "Program" stmts_json []
