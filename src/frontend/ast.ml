type binop = Add | Sub | Mul | Div

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

type expr =
  | IntLit of int
  | Id of string
  | UnaryMinus of expr
  | BinOp of expr * binop * expr
  | ErrorExpr

type decltype = Const | Mut [@@deriving show]

let string_of_decltype = function Const -> "const" | Mut -> "mut"

type statement =
  | Ret of expr
  | Decl of string * expr * decltype
  | Assignment of string * expr
  | Expr of expr
  | ErrorStmt

type program = statement list

let rec repeat s n = if n = 0 then s else s ^ repeat s (n - 1)

let rec str_expr expr ident =
  let space = repeat "  " ident in
  match expr with
  | IntLit n -> Format.sprintf "%sIntLit(%d)\n" space n
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
  match stmt with
  | Ret e -> Format.sprintf "%sRet of\n%s" space (str_expr e (ident + 1))
  | Decl (name, e, typ) ->
      Format.sprintf "%sDecl %s %s of\n%s" space (string_of_decltype typ) name
        (str_expr e (ident + 1))
  | Assignment (name, e) ->
      Format.sprintf "%sAssignment of %s of\n%s" space name
        (str_expr e (ident + 1))
  | Expr e ->
      Format.sprintf "%sExprStatement of\n%s" space (str_expr e (ident + 1))
  | ErrorStmt -> Format.sprintf "%sErrorStmt\n" space

let print_program prog =
  print_endline "Program of";
  List.iter (fun stmt -> print_string (str_stmt stmt 0)) prog
