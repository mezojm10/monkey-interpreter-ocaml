type t =
  | Illegal
  (* delimiters *)
  | Left_squirly
  | Right_squirly
  | Left_bracket
  | Right_bracket
  | Left_paren
  | Right_paren
  | Semicolon
  | Colon
  | Comma
  (* keywords *)
  | Function
  | Let
  | Return
  | If
  | Else
  | True
  | False
  (* operators *)
  | Greater_than
  | Less_than
  | Equals
  | Not_equals
  | Assign
  | Not
  | Minus
  | Plus
  | Product
  | Division
  (* identifiers + literals *)
  | Ident of string
  | Integer of string
  | String of string

val lookup_ident : string -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
