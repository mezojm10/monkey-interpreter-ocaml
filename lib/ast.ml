type node =
  | Program of stmt_list
  | Expression of expression
  | Statement of statement

and expression =
  | Integer of int
  | String of string
  | Boolean of bool
  | Identifier of identifier
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : Token.t
      ; right : expression
      }
  | GroupedExpression of expression
  | IfExpression of
      { condition : expression
      ; consequence : statement list
      ; alternative : statement list option
      }

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of stmt_list

and stmt_list = { statements : statement list }
and identifier = { identifier : string } [@@deriving show { with_path = false }]

let rec expression_to_string = function
  | Prefix { operator; right } ->
    "(" ^ Token.show operator ^ expression_to_string right ^ ")"
  | Infix { left; operator; right } ->
    "("
    ^ expression_to_string left
    ^ Token.show operator
    ^ expression_to_string right
    ^ ")"
  | Integer integer -> Int.to_string integer
  | Identifier { identifier } -> identifier
  | String string -> "\"" ^ string ^ "\""
  | Boolean bool -> string_of_bool bool
  | GroupedExpression expr -> expression_to_string expr
  | IfExpression _ as expr -> show_expression expr
;;
