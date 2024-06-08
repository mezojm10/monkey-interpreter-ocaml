type node =
  | Program of program
  | Expression of expression
  | Statement of statement
[@@deriving show { with_path = false }]

and expression =
  | Integer of int
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

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression

and program = { statements : statement list }
and identifier = { identifier : string }

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
  | Boolean bool -> string_of_bool bool
;;
