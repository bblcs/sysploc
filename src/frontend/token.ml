type kind =
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Assign
  | Semi
  | Plus
  | Minus
  | Mult
  | Div
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  | Not
  | Val
  | Var
  | Ret
  | If
  | Else
  | While
  | Break
  | Cont
  | True
  | False
  | Num of int64
  | Id of string
  | Err of string
  | ErrInvalidChar of char
  | EOF

let kind_to_json_pair = function
  | LParen -> ("LPAREN", "(")
  | RParen -> ("RPAREN", ")")
  | LBrace -> ("LBRACE", "{")
  | RBrace -> ("RBRACE", "}")
  | Assign -> ("ASSIGN", "=")
  | Semi -> ("SEMI", ";")
  | Plus -> ("PLUS", "+")
  | Minus -> ("MINUS", "-")
  | Mult -> ("MULT", "*")
  | Div -> ("DIV", "/")
  | Eq -> ("EQ", "==")
  | Ne -> ("NE", "!=")
  | Lt -> ("LT", "<")
  | Gt -> ("GT", ">")
  | Le -> ("LE", "<=")
  | Ge -> ("GE", ">=")
  | And -> ("AND", "&&")
  | Or -> ("OR", "||")
  | Not -> ("NOT", "!")
  | Val -> ("VAL", "val")
  | Var -> ("VAR", "var")
  | Ret -> ("RETURN", "return")
  | If -> ("IF", "if")
  | Else -> ("ELSE", "else")
  | While -> ("WHILE", "while")
  | Break -> ("BREAK", "break")
  | Cont -> ("CONTINUE", "continue")
  | True -> ("TRUE", "true")
  | False -> ("FALSE", "false")
  | Num n -> ("INT", Int64.to_string n)
  | Id s -> ("IDENT", s)
  | Err err -> ("ERROR", err)
  | ErrInvalidChar c -> ("ERROR", String.make 1 c)
  | EOF -> ("EOF", "")

type t = { kind : kind; pos : Loc.pos }

let show t =
  match t.kind with
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | Assign -> "Assign"
  | Semi -> "Semi"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Div -> "Div"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Le -> "Le"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"
  | Val -> "Val"
  | Var -> "Var"
  | Ret -> "Ret"
  | If -> "If"
  | Else -> "Else"
  | While -> "While"
  | Break -> "Break"
  | Cont -> "Cont"
  | True -> "True"
  | False -> "False"
  | Num n -> Format.sprintf "Num(%Ld)" n
  | Id s -> Format.sprintf "Id(%s)" s
  | Err err -> Format.sprintf "Err(%s)" err
  | ErrInvalidChar c -> Format.sprintf "ErrInvalidChar(%c)" c
  | EOF -> "EOF"

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
