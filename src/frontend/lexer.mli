(** Lexical analyzer of sysploc. *)

(** The internal state of the lexer. *)
type t

(** [make source_code] initializes the lexer with a given string. *)
val make: string -> t

(** [next lexer] Returns the next token and updated lexer state *)
val next: t -> Token.t * t
