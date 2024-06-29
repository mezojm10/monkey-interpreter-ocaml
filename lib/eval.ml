open! Base

module Object = struct
  type t =
    | Integer of int
    | True
    | False
    | String of string
    | List of t list
    | ReturnValue of t
    | Function of
        { params : Ast.identifier list
        ; body : Ast.block_stmt
        ; env : env
        }
    | Null
    | Builtin of builtin
    | Error of string

  and env =
    { store : (string, t) Hashtbl.t
    ; outer : env option
    }

  and builtin =
    | Len
    | First
    | Last
    | Rest
    | Push

  let rec show = function
    | Integer int -> Int.to_string int
    | True -> "true"
    | False -> "false"
    | String string -> "\"" ^ string ^ "\""
    | ReturnValue obj -> show obj
    | Null -> "null"
    | Function { params; body; _ } ->
      Ast.show_expression (FunctionLiteral { params; body })
    | List objs -> "[" ^ Ast.list_to_string (List.map ~f:show objs) ^ "]"
    | Builtin _ -> "builtin function"
    | Error string -> string
  ;;

  let obj_type = function
    | Integer _ -> "INT"
    | True | False -> "BOOL"
    | String _ -> "STRING"
    | Null -> "NULL"
    | ReturnValue _ -> "RETURN_VALUE"
    | Function _ -> "FUNCTION"
    | List _ -> "LIST"
    | Builtin _ -> "BUILTIN"
    | Error _ -> "ERROR"
  ;;
end

module Environment = struct
  open Object

  let empty = Hashtbl.create (module String)
  let init = { outer = None; store = empty }
  let init_enclosed outer = { outer = Some outer; store = empty }

  let rec get env key =
    match Hashtbl.find env.store key, env.outer with
    | (Some _ as value), _ -> value
    | None, Some outer -> get outer key
    | None, None -> None
  ;;

  let set env ~key ~data = Hashtbl.set env.store ~key ~data

  let extend env ~params ~args =
    let env = init_enclosed env in
    let rec loop params args =
      match params, args with
      | [], [] -> Ok env
      | Ast.{ identifier = param } :: params, arg :: args ->
        set env ~key:param ~data:arg;
        loop params args
      | _, _ -> Error "Extend failed"
    in
    loop params args
  ;;
end

let ( let* ) obj f =
  match obj with
  | Object.Error _ as error -> error
  | obj -> f obj
;;

let rec eval_node node env =
  match node with
  | Ast.Program { statements } -> eval_program statements env
  | Expression expr -> eval_expr expr env
  | Statement stmt -> eval_stmt stmt env

and eval_expr expr env : Object.t =
  match expr with
  | Integer int -> Object.Integer int
  | Boolean true -> True
  | Boolean false -> False
  | String string -> String string
  | List exps ->
    (match eval_expressions exps env with
     | [ (Error _ as value) ] -> value
     | elements -> List elements)
  | IndexExpression { list; index } ->
    let* list = eval_expr list env in
    let* index = eval_expr index env in
    eval_index_expr list index
  | Prefix { operator; right } ->
    let* right = eval_expr right env in
    eval_prefix_expr operator right
  | Infix { left; operator; right } ->
    let* left = eval_expr left env in
    let* right = eval_expr right env in
    eval_infix_expr left operator right
  | IfExpression { condition; consequence; alternative } ->
    let* condition = eval_expr condition env in
    eval_if_expr env ~consequence ~alternative ~condition
  | Identifier { identifier } -> eval_ident identifier env
  | FunctionLiteral { params; body } -> Function { params; body; env }
  | CallExpression { fn; arguments } ->
    let* fn = eval_expr fn env in
    (match eval_expressions arguments env with
     | [ (Error _ as value) ] -> value
     | arguments -> apply_function fn arguments)

and eval_ident ident env =
  match Environment.get env ident with
  | Some obj -> obj
  | None ->
    (match ident with
     | "len" -> Builtin Len
     | "first" -> Builtin First
     | "last" -> Builtin Last
     | "rest" -> Builtin Rest
     | "push" -> Builtin Push
     | _ -> Object.Error (Fmt.str "identifier not found: %s" ident))

and apply_function fn args =
  match fn with
  | Function { params; body; env } ->
    (match Environment.extend env ~args ~params with
     | Error err -> Error err
     | Ok env -> eval_function_block body env)
  | Builtin fn -> eval_builtin fn args
  | obj -> Object.Error (Fmt.str "Not a function: %s" @@ Object.obj_type obj)

and eval_stmt stmt env : Object.t =
  match stmt with
  | ExpressionStatement expr -> eval_expr expr env
  | Return expr ->
    let* obj = eval_expr expr env in
    ReturnValue obj
  | Let { name; value } ->
    let* obj = eval_expr value env in
    let obj = unwrap_return obj in
    Environment.set env ~key:name.identifier ~data:obj;
    Null
  | BlockStatement { statements } -> eval_block statements env

and eval_if_expr env ~condition ~consequence ~alternative : Object.t =
  if is_truthy condition
  then eval_block consequence.statements env
  else (
    match alternative with
    | Some alternative -> eval_block alternative.statements env
    | None -> Null)

and is_truthy = function
  | False | Null -> false
  | True -> true
  | _ -> true

and eval_function_block ({ statements } : Ast.block_stmt) env =
  let rec loop stmts =
    match stmts with
    | [] -> failwith "Empty function"
    | [ stmt ] -> eval_stmt stmt env
    | stmt :: stmts ->
      (match eval_stmt stmt env with
       | ReturnValue value -> value
       | Error _ as value -> value
       | _ -> loop stmts)
  in
  loop statements

and eval_block stmts env : Object.t =
  let rec loop stmts =
    match stmts with
    | [] -> failwith "Empty Program"
    | [ stmt ] -> eval_stmt stmt env
    | stmt :: t ->
      (match eval_stmt stmt env with
       | (ReturnValue _ | Error _) as value -> value
       | _ -> loop t)
  in
  loop stmts

and eval_program stmts env : Object.t =
  let rec loop stmts =
    match stmts with
    | [] -> failwith "Empty Program"
    | [ stmt ] -> eval_stmt stmt env |> unwrap_return
    | stmt :: t ->
      (match eval_stmt stmt env with
       | ReturnValue value -> value
       | Error _ as value -> value
       | _ -> loop t)
  in
  loop stmts

and eval_expressions exps env : Object.t list =
  let rec loop exps acc env =
    match exps with
    | [] -> List.rev acc
    | exp :: t ->
      (match eval_expr exp env with
       | Error _ as obj -> [ obj ]
       | obj -> loop t (obj :: acc) env)
  in
  loop exps [] env

and eval_prefix_expr op right =
  match op with
  | Token.Not -> eval_not_operator_expression right
  | Token.Minus -> eval_negation right
  | _ -> Error (Fmt.str "Unknown operator: %s%s" (Token.show op) (Object.show right))

and eval_index_expr list index =
  match list, index with
  | List list, Integer int -> List.nth list int |> Option.value ~default:Object.Null
  | String string, Integer int when int < String.length string ->
    String (Char.to_string @@ String.get string int)
  | String _, Integer _ -> Null
  | (List _ | String _), obj ->
    Error (Fmt.str "Unsupported use of %s as index" @@ Object.obj_type obj)
  | obj, _ ->
    Error (Fmt.str "Unsupported index operation on type: %s" @@ Object.obj_type obj)

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
  | String s1, Token.Plus, String s2 -> String (s1 ^ s2)
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

and unwrap_return = function
  | ReturnValue obj | obj -> obj

and eval_builtin fn args =
  match fn with
  | Len -> eval_builtin_len args
  | First -> eval_builtin_first args
  | Last -> eval_builtin_last args
  | Rest -> eval_builtin_rest args
  | Push -> eval_builtin_push args

and eval_builtin_len = function
  | [ String string ] -> Integer (String.length string)
  | [ List list ] -> Integer (List.length list)
  | [ ((Builtin _ | Error _ | Function _ | ReturnValue _ | Integer _ | True | False | Null)
       as obj)
    ] -> Error (Fmt.str "unsupported argument for len: %s" (Object.obj_type obj))
  | ([] | _ :: _ :: _) as args ->
    Error (Fmt.str "len requires 1 argument, got: %d" (List.length args))

and eval_builtin_first = function
  | [ List [] ] -> Null
  | [ List (h :: _) ] -> h
  | [ (( Builtin _
       | Error _
       | Function _
       | ReturnValue _
       | Integer _
       | String _
       | True
       | False
       | Null ) as obj)
    ] -> Error (Fmt.str "unsupported argument for first: %s" (Object.obj_type obj))
  | ([] | _ :: _ :: _) as args ->
    Error (Fmt.str "first requires 1 argument, got: %d" (List.length args))

and eval_builtin_last = function
  | [ List [] ] -> Null
  | [ List list ] -> List.last_exn list
  | [ (( Builtin _
       | Error _
       | Function _
       | ReturnValue _
       | Integer _
       | String _
       | True
       | False
       | Null ) as obj)
    ] -> Error (Fmt.str "unsupported argument for last: %s" (Object.obj_type obj))
  | ([] | _ :: _ :: _) as args ->
    Error (Fmt.str "last requires 1 argument, got: %d" (List.length args))

and eval_builtin_rest = function
  | [ List [] ] -> Null
  | [ List (_ :: t) ] -> List t
  | [ (( Builtin _
       | Error _
       | Function _
       | ReturnValue _
       | Integer _
       | String _
       | True
       | False
       | Null ) as obj)
    ] -> Error (Fmt.str "unsupported argument for rest: %s" (Object.obj_type obj))
  | ([] | _ :: _ :: _) as args ->
    Error (Fmt.str "rest requires 1 argument, got: %d" (List.length args))

and eval_builtin_push = function
  | [ List []; obj ] -> List [ obj ]
  | [ List list; obj ] -> List (list @ [ obj ])
  | [ (( Builtin _
       | Error _
       | Function _
       | ReturnValue _
       | Integer _
       | String _
       | True
       | False
       | Null ) as obj)
    ; _
    ] -> Error (Fmt.str "unsupported argument for push: %s" (Object.obj_type obj))
  | ([] | [ _ ] | _ :: _ :: _ :: _) as args ->
    Error (Fmt.str "push requires 2 argument, got: %d" (List.length args))
;;

module Tests = struct
  let test_eval input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    match Parser.parse parser with
    | Error error -> Stdio.print_endline error
    | Ok node ->
      let obj = eval_node node Environment.init in
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
    let input = [ {|"this is a string";|}; {|"Hello " + "World!"|} ] in
    List.iter ~f:test_eval input;
    [%expect {|
      "this is a string"
      "Hello World!"
      |}]
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
      ; {| "Hello" + 15 |}
      ; "foobar"
      ; "1[0]"
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
      type mismatch: STRING + INT
      identifier not found: foobar
      Unsupported index operation on type: INT
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
      ; {|
        let counter = fn(x) {
          if (x > 100) {
            return true;
          } else {
            let foobar = 9999;
            counter(x + 1);
          }
        };
        counter(0);
      |}
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      5
      10
      3
      15
      5
      true
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

  let%expect_test "test builtin functions" =
    let input =
      [ {| len("") |}
      ; {| len("four") |}
      ; {| len("Mazin"); |}
      ; "len([])"
      ; "len([1, 2, 3])"
      ; "first([1, 2, 3])"
      ; "last([1, 2, 3])"
      ; "first([2])"
      ; "last([2])"
      ; "first([])"
      ; "last([])"
      ; "rest([1, 2, 3, 4])"
      ; "rest([1])"
      ; "rest([])"
      ; "push([], 1)"
      ; "push([1], 2)"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      0
      4
      5
      0
      3
      1
      3
      2
      2
      null
      null
      [2, 3, 4]
      []
      null
      [1]
      [1, 2]
      |}]
  ;;

  let%expect_test "test builtin function errors" =
    let input =
      [ "len();"
      ; "len(5);"
      ; "len(true);"
      ; {| len(5 + 5, "mazin"); |}
      ; "first(5)"
      ; "last(5)"
      ; "first(5, 6)"
      ; "last(5, 6)"
      ; "rest(5)"
      ; "rest()"
      ; "rest(5, 6)"
      ; "push()"
      ; "push([1])"
      ; "push([1], 2, 3)"
      ; "push(1, 2)"
      ]
    in
    List.iter input ~f:test_eval;
    [%expect
      {|
      len requires 1 argument, got: 0
      unsupported argument for len: INT
      unsupported argument for len: BOOL
      len requires 1 argument, got: 2
      unsupported argument for first: INT
      unsupported argument for last: INT
      first requires 1 argument, got: 2
      last requires 1 argument, got: 2
      unsupported argument for rest: INT
      rest requires 1 argument, got: 0
      rest requires 1 argument, got: 2
      push requires 2 argument, got: 0
      push requires 2 argument, got: 1
      push requires 2 argument, got: 3
      unsupported argument for push: INT
      |}]
  ;;

  let%expect_test "test index expressions" =
    let input =
      [ {| [0, 1, 2][0] |}
      ; {| [0, 1 + 1][1] |}
      ; {| [0, 1 + 1][5] |}
      ; {| [["Mazin"]][0][0] |}
      ; {| ["Mazin"][0][4] |}
      ; {| ["Mazin"][0][5] |}
      ]
    in
    List.iter input ~f:test_eval;
    [%expect {|
      0
      2
      null
      "Mazin"
      "n"
      null
      |}]
  ;;
end
