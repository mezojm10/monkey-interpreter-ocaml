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
[@@deriving show, eq]

let lookup_ident = function
  | "fn" -> Function
  | "if" -> If
  | "else" -> Else
  | "let" -> Let
  | "return" -> Return
  | "true" -> True
  | "false" -> False
  | str -> Ident str
;;

let show = function
  | Left_squirly -> "{"
  | Right_squirly -> "}"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Semicolon -> ";"
  | Colon -> ":"
  | Comma -> ","
  (* keywords *)
  | Function -> "fn"
  | Let -> "let"
  | Return -> "return"
  | If -> "if"
  | Else -> "else"
  | True -> "true"
  | False -> "false"
  (* operators *)
  | Greater_than -> ">"
  | Less_than -> "<"
  | Equals -> "=="
  | Not_equals -> "!="
  | Assign -> "="
  | Not -> "!"
  | Minus -> "-"
  | Plus -> "+"
  | Product -> "*"
  | Division -> "/"
  (* identifiers + literals *)
  | Ident string | Integer string | String string -> string
  | Illegal -> "ILLEGAL"
;;
