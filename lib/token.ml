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
[@@deriving show, eq]

let lookup_ident = function
  | "fn" -> FUNCTION
  | "if" -> IF
  | "else" -> ELSE
  | "let" -> LET
  | "return" -> RETURN
  | "true" -> TRUE
  | "false" -> FALSE
  | str -> IDENT str
;;
