open Base

let ( let* ) res f = Result.bind res ~f

type precedence =
  [ `LOWEST
  | `EQUALS
  | `LESSGREATER
  | `SUM
  | `PRODUCT
  | `PREFIX
  | `CALL
  ]
[@@deriving ord]

let get_precedence = function
  | Token.Equals | Token.Not_equals -> `EQUALS
  | Token.Less_than | Token.Greater_than -> `LESSGREATER
  | Token.Plus | Token.Minus -> `SUM
  | Token.Product | Token.Division -> `PRODUCT
  | _ -> `LOWEST
;;

type t =
  { lexer : Lexer.t
  ; curToken : Token.t option
  ; nextToken : Token.t option
  }

let next_token parser =
  let lexer, nextToken = Lexer.next_token parser.lexer in
  (* Fmt.pr *)
  (*   "Current token is: %s\n" *)
  (*   (Token.show (Option.value ~default:Token.Illegal parser.nextToken)); *)
  { lexer; nextToken; curToken = parser.nextToken }
;;

let init lexer = { lexer; curToken = None; nextToken = None } |> next_token |> next_token

let rec parse parser =
  let rec loop parser statements =
    match parser.curToken with
    | Some _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> loop (next_token parser) (stmt :: statements)
       | Error msg -> Error (Fmt.str "Something went wrong: %s" msg))
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = loop parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.curToken with
  | Some Token.Let -> parse_let (next_token parser)
  | Some Token.Return -> parse_return (next_token parser)
  | Some _ -> parse_expression_statement parser
  | _ ->
    Stdio.print_endline (Token.show (Option.value_exn parser.curToken));
    failwith "end"

and parse_let parser =
  let* parser, ident = parse_ident parser in
  let* parser = expect_assign parser in
  let* parser, expr = parse_expr (next_token parser) `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { name = ident; value = expr })

and parse_return parser =
  let* parser, expr = parse_expr parser `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_expression_statement parser =
  let* parser, expr = parse_expr parser `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_expr parser precedence =
  (* Fmt.pr "parsing expression: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
  let* parser, left_exp = prefix_parse parser in
  let rec loop parser left =
    (* Fmt.pr "LOOP: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
    match peek_token_is parser Token.Semicolon with
    | false when compare_precedence precedence (peek_precedence parser) < 0 ->
      (* Fmt.pr "BRANCH 1: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
      (match infix_parse parser with
       | Some (f, op) ->
         let* parser, left = f (next_token parser) ~op ~left in
         loop parser left
       | None -> Ok (parser, left))
    | _ ->
      (* Fmt.pr "BRANCH 2: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
      Ok (parser, left)
  in
  loop parser left_exp

and peek_precedence parser =
  match parser.nextToken with
  | Some tok -> get_precedence tok
  | None -> `LOWEST

and peek_token_is parser token =
  match parser.nextToken with
  | Some tok -> Token.equal token tok
  | None -> false

and expr_parse_ident parser =
  match parser.curToken with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.Identifier { identifier })
  | _ -> Error "Expected to read identifier"

and expect_assign parser =
  match parser.nextToken with
  | Some Token.Assign -> Ok (next_token parser)
  | _ -> Error "Expected ="

and chomp_semicolon parser =
  match parser.nextToken with
  | Some Token.Semicolon -> next_token parser
  | _ -> parser

and prefix_parse parser =
  (* Fmt.pr *)
  (*   "Fetching prefix parse function for: %s\n" *)
  (*   (Token.show (Option.value_exn parser.curToken)); *)
  let* parser, expr =
    match parser.curToken with
    | Some (Token.Ident _) -> expr_parse_ident parser
    | Some (Token.Integer _) -> parse_integer parser
    | Some (Token.True | Token.False) -> expr_parse_bool parser
    | Some ((Token.Not | Token.Minus) as op) -> expr_parse_prefix (next_token parser) op
    | Some tok ->
      Error (Fmt.str "Invalid expression, expected prefix, got %s" (Token.show tok))
    | _ -> Error "Invalid expression"
  in
  (* Fmt.pr "Parsed expression: %s\n" @@ Ast.show_expression expr; *)
  Ok (parser, expr)

and infix_parse parser =
  (* Fmt.pr *)
  (*   "Fetching infix parse function for: %s\n" *)
  (*   (Token.show (Option.value_exn parser.nextToken)); *)
  match parser.nextToken with
  | Some
      (( Token.Plus
       | Token.Minus
       | Token.Product
       | Token.Division
       | Token.Greater_than
       | Token.Less_than
       | Token.Equals
       | Token.Not_equals ) as op) -> Some (expr_parse_infix, op)
  | _ -> None

and expr_parse_prefix parser op =
  let* parser, expr = parse_expr parser `PREFIX in
  Ok (parser, Ast.Prefix { operator = op; right = expr })

and expr_parse_infix parser ~op ~left =
  (* Fmt.pr "parsing infix operator: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
  let precedence = get_precedence op in
  let* parser, right = parse_expr (next_token parser) precedence in
  Ok (parser, Ast.Infix { left; operator = op; right })

and parse_ident parser =
  match parser.curToken with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.{ identifier })
  | _ -> Error "Expected to read identifier"

and parse_integer parser =
  match parser.curToken with
  | Some (Token.Integer integer) ->
    (match Int.of_string_opt integer with
     | Some integer -> Ok (parser, Ast.Integer integer)
     | None -> Error (Fmt.str "Could not parse %s as integer" integer))
  | Some t -> Error (Fmt.str "Expected Token.Integer, found: %s" (Token.show t))
  | None -> Error "Reached end of file, but was expecting integer"

and expr_parse_bool parser =
  match parser.curToken with
  | Some Token.True -> Ok (parser, Ast.Boolean true)
  | Some Token.False -> Ok (parser, Ast.Boolean false)
  | Some t -> Error (Fmt.str "Expected a boolean, found %s" (Token.show t))
  | None -> Error "Reached end of file, but was expecting boolean"
;;

let string_of_statement stmt expression_to_string =
  match stmt with
  | Ast.Let stmt ->
    Fmt.str
      "LET: let %s = %s"
      (Ast.show_identifier stmt.name)
      (expression_to_string stmt.value)
  | Ast.Return expr -> Fmt.str "RETURN: return %s" (expression_to_string expr)
  | Ast.ExpressionStatement expr -> Fmt.str "EXPR: %s" (expression_to_string expr)
;;

(* (show_expression stmt.value) *)

(* and string_of_ident ident = Ast.(ident.identifier) *)

let print_node node expression_to_string =
  match node with
  | Ast.Program program ->
    Fmt.pr "Program: [@.";
    List.iter program.statements ~f:(fun s ->
      Fmt.pr " %s@." (string_of_statement s expression_to_string));
    Fmt.pr "]@."
  | _ -> failwith "oops"
;;

module Tests = struct
  let expect_program input expression_to_string =
    let lexer = Lexer.init input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> print_node program expression_to_string
    | Error msg -> Stdio.print_endline msg
  ;;

  let%expect_test "Test let statements" =
    let input =
      {|
    let x = 5;
    let y = 10;
    let foobar = 838383;
    let foo = true;
    let bar = false;
  |}
    in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       LET: let { identifier = "x" } = (Integer 5)
       LET: let { identifier = "y" } = (Integer 10)
       LET: let { identifier = "foobar" } = (Integer 838383)
       LET: let { identifier = "foo" } = (Boolean true)
       LET: let { identifier = "bar" } = (Boolean false)
      ]
      |}]
  ;;

  let%expect_test "Test return statements" =
    let input = {|
    return 5;
    return true;
    return x;
  |} in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       RETURN: return (Integer 5)
       RETURN: return (Boolean true)
       RETURN: return (Identifier { identifier = "x" })
      ]
      |}]
  ;;

  let%expect_test "Test identifier expression" =
    let input = {|
    foobar;
    |} in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       EXPR: (Identifier { identifier = "foobar" })
      ]
      |}]
  ;;

  let%expect_test "Test integer literal expression" =
    let input = {|
    5;
    |} in
    expect_program input Ast.show_expression;
    [%expect {|
      Program: [
       EXPR: (Integer 5)
      ]
      |}]
  ;;

  let%expect_test "Test boolean literal expression" =
    let input = {|
    true;
    false;
    |} in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       EXPR: (Boolean true)
       EXPR: (Boolean false)
      ]
      |}]
  ;;

  let%expect_test "Test parsing prefix expressions" =
    let input = {|
    !5;
    -15;
    !true;
    !false;
    |} in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       EXPR: Prefix {operator = Token.Not; right = (Integer 5)}
       EXPR: Prefix {operator = Token.Minus; right = (Integer 15)}
       EXPR: Prefix {operator = Token.Not; right = (Boolean true)}
       EXPR: Prefix {operator = Token.Not; right = (Boolean false)}
      ]
      |}]
  ;;

  let%expect_test "Test parsing infix expressions" =
    let input =
      {|
    5 + 5;
    5 - 5;
    5 * 5;
    5 / 5;
    5 > 5;
    5 < 5;
    5 == 5;
    5 != 5;
    true == false;
    |}
    in
    expect_program input Ast.show_expression;
    [%expect
      {|
      Program: [
       EXPR: Infix {left = (Integer 5); operator = Token.Plus; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Minus; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Product; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Division; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Greater_than; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Less_than; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Equals; right = (Integer 5)}
       EXPR: Infix {left = (Integer 5); operator = Token.Not_equals; right = (Integer 5)}
       EXPR: Infix {left = (Boolean true); operator = Token.Equals;
        right = (Boolean false)}
      ]
      |}]
  ;;

  let%expect_test "Test operator precedence parsing" =
    let input =
      {|
    -a * 5;
    !-a;
    a + b + c;
    a + b - c;
    a * b * c;
    a * b / c;
    a + b / c;
    a + b * c + d / e - f;
    3 + 4; -5 * 5;
    5 > 4 == 3 < 4;
    5 < 4 != 3 > 4;
    5 > 4 == false;
    5 < 4 == true
    3 + 4 * 5 == 3 * 1 + 4 * 5;
    |}
    in
    expect_program input Ast.expression_to_string;
    [%expect
      {|
      Program: [
       EXPR: ((-a)*5)
       EXPR: (!(-a))
       EXPR: ((a+b)+c)
       EXPR: ((a+b)-c)
       EXPR: ((a*b)*c)
       EXPR: ((a*b)/c)
       EXPR: (a+(b/c))
       EXPR: (((a+(b*c))+(d/e))-f)
       EXPR: (3+4)
       EXPR: ((-5)*5)
       EXPR: ((5>4)==(3<4))
       EXPR: ((5<4)!=(3>4))
       EXPR: ((5>4)==false)
       EXPR: ((5<4)==true)
       EXPR: ((3+(4*5))==((3*1)+(4*5)))
      ]
      |}]
  ;;
end
