type pos = { line : int; col : int } [@@deriving show]

(* will be used later in the parser *)
type t = { start : pos; end_ : pos } [@@deriving show]
