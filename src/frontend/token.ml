type kind =
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
  | ErrInvalidChar of char
  | EOF
[@@deriving show]

let kind_to_json_pair = function
  | LParen -> ("LPAREN", "(")
  | RParen -> ("RPAREN", ")")
  | Assign -> ("EQ", "=")
  | Semi -> ("SEMI", ";")
  | Plus -> ("PLUS", "+")
  | Minus -> ("MINUS", "-")
  | Mult -> ("MULT", "*")
  | Div -> ("DIV", "/")
  | Val -> ("VAL", "val")
  | Var -> ("VAR", "var")
  | Ret -> ("RETURN", "return")
  | Num n -> ("INT", string_of_int n)
  | Id s -> ("IDENT", s)
  | Err err -> ("ERROR", err)
  | ErrInvalidChar c -> ("ERROR", String.make 1 c)
  | EOF -> ("EOF", "")

type t = { kind : kind; pos : Loc.pos } [@@deriving show]

let to_yojson token =
  let kind, value = kind_to_json_pair token.kind in
  `Assoc
    [
      ("kind", `String kind);
      ("value", `String value);
      ("line", `Int (token.pos.line + 1));
      ("column", `Int (token.pos.col + 1));
    ]

let is_error tok =
  match tok.kind with Err _ | ErrInvalidChar _ -> true | _ -> false
