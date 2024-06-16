open! Base

module Object = struct
  type obj =
    | Integer of int
    | True
    | False
    | String of string
    | ReturnValue of obj
    | Null
    | Error of string
  [@@deriving show]

  let rec show_obj = function
    | Integer int -> Int.to_string int
    | True -> "true"
    | False -> "false"
    | String string -> "\"" ^ string ^ "\""
    | ReturnValue obj -> show_obj obj
    | Null -> "null"
    | Error string -> string
  ;;

  let obj_type = function
    | Integer _ -> "INT"
    | True | False -> "BOOL"
    | String _ -> "STRING"
    | Null -> "NULL"
    | ReturnValue _ -> "RETURN_VALUE"
    | Error _ -> "ERROR"
  ;;
end

let rec eval_node = function
  | Ast.Program { statements } -> eval_stmts statements
  | Expression expr -> eval_expr expr
  | Statement stmt -> eval_stmt stmt

and eval_expr = function
  | Integer int -> Object.Integer int
  | Boolean true -> True
  | Boolean false -> False
  | String string -> String string
  | Prefix { operator; right } ->
    (match eval_expr right with
     | Error _ as right -> right
     | right -> eval_prefix_expr operator right)
  | Infix { left; operator; right } ->
    (match eval_expr left with
     | Error _ as left -> left
     | left ->
       (match eval_expr right with
        | Error _ as right -> right
        | right -> eval_infix_expr left operator right))
  | IfExpression { condition; consequence; alternative } ->
    (match eval_expr condition with
     | Error _ as obj -> obj
     | condition -> eval_if_expr ~consequence ~alternative ~condition)
  | Identifier _ | FunctionLiteral _ | CallExpression _ -> failwith "Not implemented"

and eval_stmt = function
  | ExpressionStatement expr -> eval_expr expr
  | Return expr ->
    (match eval_expr expr with
     | Error _ as obj -> obj
     | obj -> ReturnValue obj)
  | Let _ -> failwith "Not implemented"
  | BlockStatement _ -> failwith "Not implemented"

and eval_if_expr ~condition ~consequence ~alternative =
  if is_truthy condition
  then eval_stmts consequence.statements
  else (
    match alternative with
    | Some alternative -> eval_stmts alternative.statements
    | None -> Null)

and is_truthy = function
  | False | Null -> false
  | True -> true
  | _ -> true

and eval_stmts stmts =
  let rec loop = function
    | [] -> failwith "Empty Program"
    | [ stmt ] -> eval_stmt stmt
    | stmt :: t ->
      (match eval_stmt stmt with
       | ReturnValue value -> value
       | Error _ as value -> value
       | _ -> loop t)
  in
  loop stmts

and eval_prefix_expr op right =
  match op with
  | Token.Not -> eval_not_operator_expression right
  | Token.Minus -> eval_negation right
  | _ -> Error (Fmt.str "Unknown operator: %s%s" (Token.show op) (Object.show_obj right))

and eval_not_operator_expression = function
  | True -> False
  | False | Null -> True
  | _ -> False

and eval_negation = function
  | Integer int -> Integer (-int)
  | right -> Error (Fmt.str "unknown operator: -%s" (Object.obj_type right))

and eval_infix_expr left op right =
  match left, op, right with
  | Integer left, Token.Plus, Integer right -> Integer (left + right)
  | Integer left, Token.Minus, Integer right -> Integer (left - right)
  | Integer left, Token.Product, Integer right -> Integer (left * right)
  | Integer left, Token.Division, Integer right -> Integer (left / right)
  | Integer left, Token.Greater_than, Integer right ->
    eval_native_bool_to_obj (left > right)
  | Integer left, Token.Less_than, Integer right -> eval_native_bool_to_obj (left < right)
  | Integer left, Token.Equals, Integer right -> eval_native_bool_to_obj (left = right)
  | Integer left, Token.Not_equals, Integer right ->
    eval_native_bool_to_obj (not (left = right))
  | True, Token.Equals, True -> True
  | True, Token.Equals, False -> False
  | False, Token.Equals, True -> False
  | False, Token.Equals, False -> True
  | True, Token.Not_equals, True -> False
  | True, Token.Not_equals, False -> True
  | False, Token.Not_equals, True -> True
  | False, Token.Not_equals, False -> False
  | ( Integer _
    , ( Token.Plus
      | Token.Minus
      | Token.Product
      | Token.Division
      | Token.Greater_than
      | Token.Less_than
      | Token.Equals
      | Token.Not_equals )
    , _ ) ->
    Error
      (Fmt.str
         "type mismatch: %s %s %s"
         (Object.obj_type left)
         (Token.show op)
         (Object.obj_type right))
  | ( _
    , ( Token.Plus
      | Token.Minus
      | Token.Product
      | Token.Division
      | Token.Greater_than
      | Token.Less_than
      | Token.Equals
      | Token.Not_equals )
    , Integer _ ) ->
    Error
      (Fmt.str
         "type mismatch: %s %s %s"
         (Object.obj_type left)
         (Token.show op)
         (Object.obj_type right))
  | _ ->
    Error
      (Fmt.str
         "unknown operator: %s %s %s"
         (Object.obj_type left)
         (Token.show op)
         (Object.obj_type right))

and eval_native_bool_to_obj = function
  | true -> True
  | false -> False
;;

module Tests = struct
  let test_eval input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    match Parser.parse parser with
    | Error error -> Stdio.print_endline error
    | Ok node -> Stdio.print_endline (Object.show_obj (eval_node node))
  ;;

  let%expect_test "test eval integer expression" =
    let input =
      [ "5"
      ; "10"
      ; "-5"
      ; "-10"
      ; "5 + 5 + 5 + 5 - 10"
      ; "2 * 2 * 2 * 2 * 2"
      ; "-50 + 100 + -50"
      ; "5 * 2 + 10"
      ; "5 + 2 * 10"
      ; "20 + 2 * -10"
      ; "50 / 2 * 2 + 10"
      ; "2 * (5 + 10)"
      ; "3 * 3 * 3 + 10"
      ; "3 * (3 * 3) + 10"
      ; "(5 + 10 * 2 + 15 / 3) * 2 + -10"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      5
      10
      -5
      -10
      10
      32
      0
      20
      25
      0
      60
      30
      37
      37
      50
      |}]
  ;;

  let%expect_test "test eval boolean expression" =
    let input = [ "true"; "false" ] in
    List.iter input ~f:test_eval;
    [%expect {|
      true
      false
      |}]
  ;;

  let%expect_test "test eval string expression" =
    let input = {|
      "this is a string";
    |} in
    test_eval input;
    [%expect {| "this is a string" |}]
  ;;

  let%expect_test "test not operator" =
    let input = [ "!true"; "!false" ] in
    List.iter input ~f:test_eval;
    [%expect {|
      false
      true
      |}]
  ;;

  let%expect_test "test eval boolean expression" =
    let input =
      [ "true"
      ; "false"
      ; "1 < 2"
      ; "1 > 2"
      ; "1 < 1"
      ; "1 > 1"
      ; "1 == 1"
      ; "1 != 1"
      ; "1 == 2"
      ; "1 != 2"
      ; "true == true"
      ; "false == false"
      ; "true == false"
      ; "false == true"
      ; "true != true"
      ; "true != false"
      ; "false != true"
      ; "false != false"
      ; "(1 < 2) == true"
      ; "(1 < 2) == false"
      ; "(1 > 2) == true"
      ; "(1 > 2) == false"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      true
      false
      true
      false
      false
      false
      true
      false
      false
      true
      true
      true
      false
      false
      false
      true
      true
      false
      true
      false
      false
      true
      |}]
  ;;

  let%expect_test "test if else expressions" =
    let input =
      [ "if (true) { 10 }"
      ; "if (false) { 10 }"
      ; "if (1) { 10 }"
      ; "if (1 < 2) { 10 }"
      ; "if (1 > 2) { 10 }"
      ; "if (1 > 2) { 10 } else { 20 }"
      ; "if (1 < 2) { 10 } else { 20 }"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      10
      null
      10
      10
      null
      20
      10
      |}]
  ;;

  let%expect_test "test return statements" =
    let input =
      [ "return 10;"
      ; "return 10; 9"
      ; "return 2 * 5; 9"
      ; "9; return 2 * 5; 9;"
      ; "if (10 > 1) { if (10 > 1) { return 10;} return 1;}"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      10
      10
      10
      10
      10
      |}]
  ;;

  let%expect_test "test error handling" =
    let input =
      [ "5 + true;"
      ; "5 + true; 5"
      ; "true + 5; 5"
      ; "-true"
      ; "true + false;"
      ; "5; true + false; 5;"
      ; "if (10 > 1) { true + false; }"
      ; "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      type mismatch: INT + BOOL
      type mismatch: INT + BOOL
      type mismatch: BOOL + INT
      unknown operator: -BOOL
      unknown operator: BOOL + BOOL
      unknown operator: BOOL + BOOL
      unknown operator: BOOL + BOOL
      unknown operator: BOOL + BOOL
      |}]
  ;;
end
