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
  | EOF
[@@deriving show]

(* makes a kind, value pair for json suite *)
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
