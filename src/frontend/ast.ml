type binop = Add | Sub | Mul | Div [@@deriving show]

type expr =
  | IntLit of int
  | Id of string
  | UnaryMinus of expr
  | BinOp of expr * binop * expr
[@@deriving show]

type decltype = Const | Mut [@@deriving show]

type statement =
  | Ret of expr
  | Decl of string * expr * decltype
  | Assignment of string * expr
  | Expr of expr
[@@deriving show]

type program = statement list [@@deriving show]
