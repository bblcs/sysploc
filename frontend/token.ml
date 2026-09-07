type token_type =
  | LParen
  | RParen
  | Assign
  | Semi
  | Plus
  | Minus
  | Mult
  | Div
  | Val
  | Var
  | Ret
  | Num of int
  | Id of string
  | Err of string
  | EOF
[@@deriving show]

type position = { line : int; col : int } [@@deriving show]
type token = { kind : token_type; pos : position } [@@deriving show]
