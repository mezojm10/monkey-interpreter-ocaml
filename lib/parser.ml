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
  | `INDEX
  ]
[@@deriving ord]

let get_precedence = function
  | Token.Equals | Token.Not_equals -> `EQUALS
  | Token.Less_than | Token.Greater_than -> `LESSGREATER
  | Token.Plus | Token.Minus -> `SUM
  | Token.Product | Token.Division -> `PRODUCT
  | Token.Left_paren -> `CALL
  | Token.Left_bracket -> `INDEX
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
  | Some Token.Left_squirly -> parse_block_statement parser
  | Some _ -> parse_expression_statement parser
  | _ ->
    Stdio.print_endline (Token.show (Option.value_exn parser.curToken));
    failwith "end"

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

and parse_let parser =
  let* parser, ident = parse_ident parser in
  let* parser = expect parser Token.Assign in
  let* parser, expr = parse_expr (next_token parser) `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { name = ident; value = expr })

and parse_return parser =
  let* parser, expr = parse_expr parser `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_block_statement parser =
  let* parser, block = parse_block parser in
  Ok (parser, Ast.BlockStatement block)

and parse_block parser : (t * Ast.block_stmt, string) Result.t =
  let* parser =
    match parser.curToken with
    | Some Token.Left_squirly -> Ok (next_token parser)
    | Some t -> Error (Fmt.str "Expected {, got: %s" (Token.show t))
    | None -> Error "Expected {, got EOF"
  in
  let rec loop parser statements =
    match parser.curToken with
    | Some Token.Right_squirly -> Ok (parser, List.rev statements)
    | Some _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> loop (next_token parser) (stmt :: statements)
       | Error msg -> Error (Fmt.str "Something went wrong: %s" msg))
    | None -> Error "Expected }, got EOF"
  in
  let* parser, statements = loop parser [] in
  Ok (parser, Ast.{ statements })

and parse_expression_statement parser =
  let* parser, expr = parse_expr parser `LOWEST in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and expect parser expected =
  match parser.nextToken with
  | Some token when Token.equal token expected -> Ok (next_token parser)
  | Some token ->
    Error (Fmt.str "Expected %s, got: %s" (Token.show expected) (Token.show token))
  | None -> Error (Fmt.str "Expected %s, got: EOF" (Token.show expected))

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
    | Some (Token.Integer _) -> expr_parse_integer parser
    | Some (Token.String _) -> expr_parse_string parser
    | Some (Token.True | Token.False) -> expr_parse_bool parser
    | Some ((Token.Not | Token.Minus) as op) -> expr_parse_prefix (next_token parser) op
    | Some Token.Left_paren -> expr_parse_grouped (next_token parser)
    | Some Token.If -> expr_parse_if parser
    | Some Token.Function -> expr_parse_function parser
    | Some Token.Left_bracket -> expr_parse_list_literal parser
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
  | Some (Token.Left_paren as op) -> Some (expr_parse_call, op)
  | Some (Token.Left_bracket as op) -> Some (expr_parse_index, op)
  | _ -> None

and expr_parse_prefix parser op =
  let* parser, expr = parse_expr parser `PREFIX in
  Ok (parser, Ast.Prefix { operator = op; right = expr })

and expr_parse_grouped parser =
  let* parser, expr = parse_expr parser `LOWEST in
  let* parser = expect parser Token.Right_paren in
  Ok (parser, expr)

and expr_parse_list_literal parser =
  match parse_expression_list parser Token.Right_bracket with
  | Error "EOF" -> Error "Reached EOF when parsing list"
  | Error "consecutive-commas" -> Error "2 consecutive commas when parsing list"
  | Error _ as err -> err
  | Ok (parser, elements) -> Ok (parser, Ast.List elements)

and expr_parse_infix parser ~op ~left =
  (* Fmt.pr "parsing infix operator: %s\n" (Token.show (Option.value_exn parser.curToken)); *)
  let precedence = get_precedence op in
  let* parser, right = parse_expr (next_token parser) precedence in
  Ok (parser, Ast.Infix { left; operator = op; right })

and parse_params parser =
  let rec loop parser params =
    match parser.nextToken with
    | Some Token.Right_paren -> Ok (parser, List.rev params)
    | Some (Token.Ident identifier) ->
      loop (next_token parser) (Ast.{ identifier } :: params)
    | Some Token.Comma -> loop (next_token parser) params
    | Some tok ->
      Error (Fmt.str "Unexpected token found in function literal: %s" (Token.show tok))
    | None -> Error "Reached EOF when parsing function literal"
  in
  loop parser []

and expr_parse_function parser =
  let* parser = expect parser Token.Left_paren in
  let* parser, params = parse_params parser in
  let* parser = expect parser Token.Right_paren in
  let* parser, body = parse_block (next_token parser) in
  Ok (parser, Ast.FunctionLiteral { params; body })

and parse_expression_list parser end_token =
  let rec loop parser args =
    match parser.nextToken with
    | Some Token.Comma when cur_token_is parser Token.Comma -> Error "consecutive-commas"
    | Some Token.Comma -> loop (next_token parser) args
    | Some token when Token.(equal token end_token) ->
      Ok (next_token parser, List.rev args)
    | Some _ ->
      let* parser, expr = parse_expr (next_token parser) `LOWEST in
      expr :: args |> loop parser
    | None -> Error "EOF"
  in
  loop parser []

and expr_parse_call parser ~op:_op ~left =
  match parse_expression_list parser Token.Right_paren with
  | Error "EOF" -> Error "Reached EOF when parsing call expression"
  | Error "consecutive-commas" ->
    Error "2 consecutive commas when parsing call expression"
  | Error _ as err -> err
  | Ok (parser, arguments) -> Ok (parser, Ast.CallExpression { fn = left; arguments })

and expr_parse_index parser ~op:_op ~left =
  let* parser, index = parse_expr (next_token parser) `LOWEST in
  let* parser = expect parser Token.Right_bracket in
  Ok (parser, Ast.IndexExpression { list = left; index })

and expr_parse_if parser =
  let* parser = expect parser Token.Left_paren in
  let* parser, condition = parse_expr (next_token parser) `LOWEST in
  let* parser = expect parser Token.Right_paren in
  let* parser, consequence = parse_block (next_token parser) in
  let* parser, alternative = parse_else parser in
  Ok (parser, Ast.IfExpression { condition; consequence; alternative })

and parse_else parser =
  match parser.nextToken with
  | Some Token.Else ->
    let* parser = expect (next_token parser) Token.Left_squirly in
    let* parser, alternative = parse_block parser in
    Ok (parser, Some alternative)
  | _ -> Ok (parser, None)

and parse_ident parser =
  match parser.curToken with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.{ identifier })
  | _ -> Error "Expected to read identifier"

and expr_parse_ident parser =
  match parser.curToken with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.Identifier { identifier })
  | _ -> Error "Expected to read identifier"

and expr_parse_integer parser =
  match parser.curToken with
  | Some (Token.Integer integer) ->
    (match Int.of_string_opt integer with
     | Some integer -> Ok (parser, Ast.Integer integer)
     | None -> Error (Fmt.str "Could not parse %s as integer" integer))
  | Some t -> Error (Fmt.str "Expected Token.Integer, found: %s" (Token.show t))
  | None -> Error "Reached end of file, but was expecting integer"

and expr_parse_string parser =
  match parser.curToken with
  | Some (Token.String string) -> Ok (parser, Ast.String string)
  | Some t -> Error (Fmt.str "Expected Token.String, found: %s" (Token.show t))
  | None -> Error "Reached end of file, but was expecting integer"

and expr_parse_bool parser =
  match parser.curToken with
  | Some Token.True -> Ok (parser, Ast.Boolean true)
  | Some Token.False -> Ok (parser, Ast.Boolean false)
  | Some t -> Error (Fmt.str "Expected a boolean, found %s" (Token.show t))
  | None -> Error "Reached end of file, but was expecting boolean"

and peek_precedence parser =
  match parser.nextToken with
  | Some tok -> get_precedence tok
  | None -> `LOWEST

and cur_token_is parser token =
  match parser.curToken with
  | Some tok -> Token.equal token tok
  | None -> false

and peek_token_is parser token =
  match parser.nextToken with
  | Some tok -> Token.equal token tok
  | None -> false
;;

module Tests = struct
  let expect_program input =
    let lexer = Lexer.init input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> Ast.pp_node Stdlib.Format.std_formatter program
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
    let name = "John";
    let names = ["Mazin", "Khalid"];
    [1 * 1, 2 + 2][0]
    if (x == y) {
      let x = 3;
      let y = 4;
    } else {
      let x = 0;
      let y = 0;
    }
    fn() { return false; }
    fn(x) { return x; }
    fn(x, y) { x + y; }
  |}
    in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [Let {name = { identifier = "x" }; value = (Integer 5)};
             Let {name = { identifier = "y" }; value = (Integer 10)};
             Let {name = { identifier = "foobar" }; value = (Integer 838383)};
             Let {name = { identifier = "foo" }; value = (Boolean true)};
             Let {name = { identifier = "bar" }; value = (Boolean false)};
             Let {name = { identifier = "name" }; value = (String "John")};
             Let {name = { identifier = "names" };
               value = (List [(String "Mazin"); (String "Khalid")])};
             (ExpressionStatement
                IndexExpression {
                  list =
                  (List
                     [Infix {left = (Integer 1); operator = Token.Product;
                        right = (Integer 1)};
                       Infix {left = (Integer 2); operator = Token.Plus;
                         right = (Integer 2)}
                       ]);
                  index = (Integer 0)});
             (ExpressionStatement
                IfExpression {
                  condition =
                  Infix {left = (Identifier { identifier = "x" });
                    operator = Token.Equals;
                    right = (Identifier { identifier = "y" })};
                  consequence =
                  { statements =
                    [Let {name = { identifier = "x" }; value = (Integer 3)};
                      Let {name = { identifier = "y" }; value = (Integer 4)}]
                    };
                  alternative =
                  (Some { statements =
                          [Let {name = { identifier = "x" }; value = (Integer 0)};
                            Let {name = { identifier = "y" }; value = (Integer 0)}]
                          })});
             (ExpressionStatement
                FunctionLiteral {params = [];
                  body = { statements = [(Return (Boolean false))] }});
             (ExpressionStatement
                FunctionLiteral {params = [{ identifier = "x" }];
                  body =
                  { statements = [(Return (Identifier { identifier = "x" }))] }});
             (ExpressionStatement
                FunctionLiteral {
                  params = [{ identifier = "x" }; { identifier = "y" }];
                  body =
                  { statements =
                    [(ExpressionStatement
                        Infix {left = (Identifier { identifier = "x" });
                          operator = Token.Plus;
                          right = (Identifier { identifier = "y" })})
                      ]
                    }})
             ]
           })
      |}]
  ;;

  let%expect_test "Test return statements" =
    let input =
      {|
    return 5;
    return true;
    return x;
    return "Test";
    if ("test" == "test") {
      return true;
    }
  |}
    in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(Return (Integer 5)); (Return (Boolean true));
             (Return (Identifier { identifier = "x" })); (Return (String "Test"));
             (ExpressionStatement
                IfExpression {
                  condition =
                  Infix {left = (String "test"); operator = Token.Equals;
                    right = (String "test")};
                  consequence = { statements = [(Return (Boolean true))] };
                  alternative = None})
             ]
           })
      |}]
  ;;

  let%expect_test "Test identifier expression" =
    let input = {|
    foobar;
    |} in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(ExpressionStatement (Identifier { identifier = "foobar" }))] })
      |}]
  ;;

  let%expect_test "Test integer literal expression" =
    let input = {|
    5;
    |} in
    expect_program input;
    [%expect {| (Program { statements = [(ExpressionStatement (Integer 5))] }) |}]
  ;;

  let%expect_test "Test call expression parsing" =
    let input = {|
    add(1, 2 * 3, 4 + 5);
    |} in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(ExpressionStatement
               CallExpression {fn = (Identifier { identifier = "add" });
                 arguments =
                 [(Integer 1);
                   Infix {left = (Integer 2); operator = Token.Product;
                     right = (Integer 3)};
                   Infix {left = (Integer 4); operator = Token.Plus;
                     right = (Integer 5)}
                   ]})
             ]
           })
      |}]
  ;;

  let%expect_test "Test boolean literal expression" =
    let input = {|
    true;
    false;
    |} in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(ExpressionStatement (Boolean true));
             (ExpressionStatement (Boolean false))]
           })
      |}]
  ;;

  let%expect_test "Test parsing prefix expressions" =
    let input = {|
    !5;
    -15;
    !true;
    !false;
    |} in
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(ExpressionStatement Prefix {operator = Token.Not; right = (Integer 5)});
             (ExpressionStatement
                Prefix {operator = Token.Minus; right = (Integer 15)});
             (ExpressionStatement
                Prefix {operator = Token.Not; right = (Boolean true)});
             (ExpressionStatement
                Prefix {operator = Token.Not; right = (Boolean false)})
             ]
           })
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
    expect_program input;
    [%expect
      {|
      (Program
         { statements =
           [(ExpressionStatement
               Infix {left = (Integer 5); operator = Token.Plus;
                 right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Minus;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Product;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Division;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Greater_than;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Less_than;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Equals;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Integer 5); operator = Token.Not_equals;
                  right = (Integer 5)});
             (ExpressionStatement
                Infix {left = (Boolean true); operator = Token.Equals;
                  right = (Boolean false)})
             ]
           })
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
    (a + b) * c;
    1 + (2 + 3) + 4;
    2 / (5 + 5);
    -(5 + 5);
    a + add(b * c) + d;
    add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
    add(a + b + c * d / f + g);
    add(a * b[2], b[1], 2 * [1, 2][1])
    a * [1, 2, 3, 4][b * c] * d
    |}
    in
    match input |> Lexer.init |> init |> parse with
    | Error error -> Stdio.print_endline error
    | Ok node ->
      Stdio.print_endline (Ast.show_node node);
      [%expect
        {|
        ((-a) * 5);
        (!(-a));
        ((a + b) + c);
        ((a + b) - c);
        ((a * b) * c);
        ((a * b) / c);
        (a + (b / c));
        (((a + (b * c)) + (d / e)) - f);
        (3 + 4);
        ((-5) * 5);
        ((5 > 4) == (3 < 4));
        ((5 < 4) != (3 > 4));
        ((5 > 4) == false);
        ((5 < 4) == true);
        ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));
        ((a + b) * c);
        ((1 + (2 + 3)) + 4);
        (2 / (5 + 5));
        (-(5 + 5));
        ((a + add((b * c))) + d);
        add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));
        add((((a + b) + ((c * d) / f)) + g));
        add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));
        ((a * ([1, 2, 3, 4][(b * c)])) * d);
        |}]
  ;;
end
