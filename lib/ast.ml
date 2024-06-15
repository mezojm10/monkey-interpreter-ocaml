open! Base

type node =
  | Program of block_stmt
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
      ; consequence : block_stmt
      ; alternative : block_stmt option
      }
  | FunctionLiteral of
      { params : identifier list
      ; body : block_stmt
      }
  | CallExpression of
      { fn : expression
      ; arguments : expression list
      }

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block_stmt

and block_stmt = { statements : statement list }
and identifier = { identifier : string } [@@deriving show { with_path = false }]

let list_to_string list =
  let rec loop list acc =
    match list with
    | [] -> acc
    | [ x ] -> acc ^ x
    | x :: t -> acc ^ x ^ ", " |> loop t
  in
  loop list ""
;;

let show_identifier ident = ident.identifier

let rec show_expression = function
  | Prefix { operator; right } -> "(" ^ Token.show operator ^ show_expression right ^ ")"
  | Infix { left; operator; right } ->
    "("
    ^ show_expression left
    ^ " "
    ^ Token.show operator
    ^ " "
    ^ show_expression right
    ^ ")"
  | Integer integer -> Int.to_string integer
  | Identifier { identifier } -> identifier
  | String string -> "\"" ^ string ^ "\""
  | Boolean bool -> Bool.to_string bool
  | GroupedExpression expr -> show_expression expr
  | IfExpression _ as expr -> show_expression expr
  | FunctionLiteral { params; body } ->
    "fn("
    ^ list_to_string (List.map ~f:show_identifier params)
    ^ ")"
    ^ show_block_stmt body
  | CallExpression { fn; arguments } ->
    show_expression fn
    ^ "("
    ^ list_to_string (List.map ~f:show_expression arguments)
    ^ ")"
;;

let rec show_statement = function
  | Let { name; value } ->
    "let " ^ show_identifier name ^ " = " ^ show_expression value ^ ";"
  | Return expr -> "return " ^ show_expression expr ^ ";"
  | ExpressionStatement expr -> show_expression expr ^ ";"
  | BlockStatement { statements } ->
    List.fold statements ~init:"{\n" ~f:(fun acc statement ->
      acc ^ show_statement statement ^ "\n")
    ^ "\n}"
;;

let show_node = function
  | Program { statements } ->
    List.fold statements ~init:"" ~f:(fun acc stmt -> acc ^ show_statement stmt ^ "\n")
  | Expression expr -> show_expression expr
  | Statement stmt -> show_statement stmt
;;
