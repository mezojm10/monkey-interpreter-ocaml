type t =
  | ILLEGAL
  (* delimiters *)
  | LEFT_SQUIRLY
  | RIGHT_SQUIRLY
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | LEFT_PAREN
  | RIGHT_PAREN
  | SEMICOLON
  | COLON
  | COMMA
  (* keywords *)
  | FUNCTION
  | LET
  | RETURN
  | IF
  | ELSE
  | TRUE
  | FALSE
  (* operators *)
  | GREATER_THAN
  | LESS_THAN
  | EQUALS
  | NOT_EQUALS
  | ASSIGN
  | NOT
  | MINUS
  | PLUS
  | PRODUCT
  | DIVISION
  (* identifiers + literals *)
  | IDENT of string
  | INTEGER of string
  | STRING of string

val lookup_ident : string -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
