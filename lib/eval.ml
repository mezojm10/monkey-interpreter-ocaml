open! Base

module Object = struct
  type t =
    | Integer of int
    | True
    | False
    | String of string
    | ReturnValue of t
    | Function of
        { params : Ast.identifier list
        ; body : Ast.block_stmt
        ; env : env
        }
    | Null
    | Error of string

  and env =
    { store : (string, t, String.comparator_witness) Map.t
    ; outer : env option
    }

  let rec show = function
    | Integer int -> Int.to_string int
    | True -> "true"
    | False -> "false"
    | String string -> "\"" ^ string ^ "\""
    | ReturnValue obj -> show obj
    | Null -> "null"
    | Function { params; body; _ } ->
      Ast.show_expression (FunctionLiteral { params; body })
    | Error string -> string
  ;;

  let obj_type = function
    | Integer _ -> "INT"
    | True | False -> "BOOL"
    | String _ -> "STRING"
    | Null -> "NULL"
    | ReturnValue _ -> "RETURN_VALUE"
    | Function _ -> "FUNCTION"
    | Error _ -> "ERROR"
  ;;
end

module Environment = struct
  open Object

  let empty = Map.empty (module String)
  let init = { outer = None; store = empty }
  let init_inner outer = { outer = Some outer; store = empty }

  let rec get env key =
    match Map.find env.store key, env.outer with
    | (Some _ as value), _ -> value
    | None, Some outer -> get outer key
    | None, None -> None
  ;;

  let set env ~key ~data = { env with store = Map.set env.store ~key ~data }

  let extend env ~params ~args =
    let rec loop params args env =
      match params, args with
      | [], [] -> Ok env
      | Ast.{ identifier = param } :: params, arg :: args ->
        set env ~key:param ~data:arg |> loop params args
      | _, _ -> Error "Extend failed"
    in
    loop params args @@ init_inner env
  ;;
end

let rec eval_node node env =
  match node with
  | Ast.Program { statements } -> eval_program statements env
  | Expression expr -> eval_expr expr env
  | Statement stmt -> eval_stmt stmt env

and eval_expr expr env : Object.t * Object.env =
  match expr with
  | Integer int -> Object.Integer int, env
  | Boolean true -> True, env
  | Boolean false -> False, env
  | String string -> String string, env
  | Prefix { operator; right } ->
    (match eval_expr right env with
     | (Error _ as right), env -> right, env
     | right, env -> eval_prefix_expr operator right, env)
  | Infix { left; operator; right } ->
    (match eval_expr left env with
     | (Error _ as left), env -> left, env
     | left, env ->
       (match eval_expr right env with
        | (Error _ as right), env -> right, env
        | right, env -> eval_infix_expr left operator right, env))
  | IfExpression { condition; consequence; alternative } ->
    (match eval_expr condition env with
     | (Error _ as obj), env -> obj, env
     | condition -> eval_if_expr env ~consequence ~alternative ~condition)
  | Identifier { identifier } ->
    ( Environment.get env identifier
      |> Option.value
           ~default:(Object.Error (Fmt.str "identifier not found: %s" identifier))
    , env )
  | FunctionLiteral { params; body } -> Function { params; body; env }, env
  | CallExpression { fn; arguments } ->
    (match eval_expr fn env with
     | (Error _ as value), env -> value, env
     | fn, env ->
       (match eval_expressions arguments env with
        | [ (Error _ as value) ], env -> value, env
        | arguments, env -> apply_function fn arguments, env))

and apply_function fn args =
  match fn with
  | Function { params; body; env } ->
    (match Environment.extend env ~args ~params with
     | Error err -> Error err
     | Ok env -> eval_function_block body env)
  | obj -> Object.Error (Fmt.str "Not a function: %s" @@ Object.obj_type obj)

and eval_stmt stmt env =
  match stmt with
  | ExpressionStatement expr -> eval_expr expr env
  | Return expr ->
    (match eval_expr expr env with
     | (Error _ as obj), env -> obj, env
     | obj, env -> ReturnValue obj, env)
  | Let { name; value } ->
    (match eval_expr value env with
     | (Error _ as obj), env -> obj, env
     | (ReturnValue obj | obj), env ->
       Null, Environment.set env ~key:name.identifier ~data:obj)
  | BlockStatement { statements } -> eval_block statements env

and eval_if_expr env ~condition ~consequence ~alternative : Object.t * Object.env =
  if is_truthy condition
  then eval_block consequence.statements env
  else (
    match alternative with
    | Some alternative -> eval_block alternative.statements env
    | None -> Object.Null, env)

and is_truthy = function
  | (False | Null), _ -> false
  | True, _ -> true
  | _ -> true

and eval_function_block ({ statements } : Ast.block_stmt) env =
  let rec loop stmts env =
    match stmts with
    | [] -> failwith "Empty function"
    | [ stmt ] ->
      let obj, _ = eval_stmt stmt env in
      obj
    | stmt :: t ->
      (match eval_stmt stmt env with
       | ReturnValue value, _ -> value
       | (Error _ as value), _ -> value
       | _, env -> loop t env)
  in
  loop statements env

and eval_block stmts env : Object.t * Object.env =
  let rec loop stmts env =
    match stmts with
    | [] -> failwith "Empty Program"
    | [ stmt ] -> eval_stmt stmt env
    | stmt :: t ->
      (match eval_stmt stmt env with
       | ((ReturnValue _ | Error _) as value), env -> value, env
       | _, env -> loop t env)
  in
  loop stmts env

and eval_program stmts env : Object.t * Object.env =
  let rec loop stmts env =
    match stmts with
    | [] -> failwith "Empty Program"
    | [ stmt ] ->
      (match eval_stmt stmt env with
       | ReturnValue value, env -> value, env
       | obj, env -> obj, env)
    | stmt :: t ->
      (match eval_stmt stmt env with
       | ReturnValue value, env -> value, env
       | (Error _ as value), env -> value, env
       | _, env -> loop t env)
  in
  loop stmts env

and eval_expressions exps env : Object.t list * Object.env =
  let rec loop exps acc env =
    match exps with
    | [] -> List.rev acc, env
    | exp :: t ->
      (match eval_expr exp env with
       | (Error _ as obj), env -> [ obj ], env
       | obj, env -> loop t (obj :: acc) env)
  in
  loop exps [] env

and eval_prefix_expr op right =
  match op with
  | Token.Not -> eval_not_operator_expression right
  | Token.Minus -> eval_negation right
  | _ -> Error (Fmt.str "Unknown operator: %s%s" (Token.show op) (Object.show right))

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
    | Ok node ->
      let obj, _env = eval_node node Environment.init in
      Stdio.print_endline (Object.show obj)
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
      ; "let x = if (true) { return 5 }; x * 5"
      ; "let x = if (true) { 6; return 5; }; x * 5"
      ; "let x = if (false) { return 5 } else { return 6 }; x * 5"
      ; "let x = if (false) { return 5; } else { 5; return 6 }; x * 5"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      10
      10
      10
      10
      10
      25
      25
      30
      30
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
      ; "foobar"
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
      identifier not found: foobar
      |}]
  ;;

  let%expect_test "test let statements" =
    let input =
      [ "let x = 25; x;"
      ; "let x = 5 * 5; x"
      ; "let x = 5; let y = x; y;"
      ; "let x = 5; let y = x; let z = x + y + 1; z;"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      25
      25
      5
      11
      |}]
  ;;

  let%expect_test "test function object" =
    let input = [ "fn(x) { x + 2; }" ] in
    List.iter input ~f:test_eval;
    [%expect
      {|
      fn(x){ statements =
        [(ExpressionStatement
            Infix {left = (Identifier { identifier = "x" }); operator = Token.Plus;
              right = (Integer 2)})
          ]
        }
      |}]
  ;;

  let%expect_test "test function application" =
    let input =
      [ "let identity = fn(x) { x }; identity(5);"
      ; "let double = fn(x) { return x * 2 }; double(5);"
      ; "let add = fn(x, y) { x + y }; add(1, 2);"
      ; "let add = fn(x, y) { x + y }; add(5, add(5, 5))"
      ; "fn(x) { x; }(5)"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      5
      10
      3
      15
      5
      |}]
  ;;

  let%expect_test "test closures" =
    let input =
      [ {|
        let newAdder = fn(x) {
          fn(y) { x + y }
        };

        let addTwo = newAdder(2);
        addTwo(2);
        |}
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      4
      |}]
  ;;
end
