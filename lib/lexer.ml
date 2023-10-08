open Base

type t =
  { input : string
  ; read_position : int
  ; ch : char option
  }

let rec init input = { input; read_position = 0; ch = None } |> read_char

(** Advances the lexer by one character and returns the new lexer *)
and read_char l =
  if l.read_position >= String.length l.input
  then { l with ch = None }
  else (
    let ch = Some l.input.[l.read_position] in
    { ch; read_position = l.read_position + 1; input = l.input })
;;

let peek_char l =
  if l.read_position >= String.length l.input
  then None
  else Some l.input.[l.read_position]
;;

let rec skip_whitespace l =
  match l.ch with
  | None -> l
  | Some ch when Char.is_whitespace ch -> skip_whitespace (read_char l)
  | _ -> l
;;

(** Reads until EOF or until encountering a character that's not _ nor an alphabetic char *)
let rec read_identifier_aux l aux =
  match l.ch with
  | None -> l, aux
  | Some c when Char.(is_alpha c || c = '_') ->
    aux ^ Char.to_string c |> read_identifier_aux (read_char l)
  | Some _ -> l, aux
;;

let read_identifier l =
  let l, ident = read_identifier_aux l "" in
  (* Check if the identifier is a keyword *)
  l, Token.lookup_ident ident
;;

(** Reads until [EOF] or until a non-digit char is encountered *)
let rec read_integer_aux l aux =
  match l.ch with
  | None -> l, aux
  | Some c when Char.is_digit c ->
    aux ^ Char.to_string c |> read_integer_aux (read_char l)
  | Some _ -> l, aux
;;

let read_integer l =
  let l, int_lit = read_integer_aux l "" in
  l, Token.INTEGER int_lit
;;

(** Just keeps reading until the terminating ['"'] or reaching EOF *)
let rec read_string_aux l aux =
  match l.ch with
  | None -> l, None
  | Some c when Char.(c <> '"') -> aux ^ Char.to_string c |> read_string_aux (read_char l)
  | Some _ -> l, Some (Token.STRING aux)
;;

let read_string l =
  let l = read_char l in
  match read_string_aux l "" with
  (* If None was returned, that means we reached EOF without the string being terminated *)
  | l, None -> l, Token.ILLEGAL
  | l, Some token -> read_char l, token
;;

(* When lexing any token, the lexer should advance one character after the end of the token *)
let next_token l =
  let l = skip_whitespace l in
  let open Token in
  match l.ch with
  | None -> l, None
  | Some ch ->
    let l, token =
      match ch with
      | '[' -> read_char l, LEFT_BRACKET
      | ']' -> read_char l, RIGHT_BRACKET
      | '(' -> read_char l, LEFT_PAREN
      | ')' -> read_char l, RIGHT_PAREN
      | '{' -> read_char l, LEFT_SQUIRLY
      | '}' -> read_char l, RIGHT_SQUIRLY
      | ':' -> read_char l, COLON
      | ';' -> read_char l, SEMICOLON
      | ',' -> read_char l, COMMA
      | '+' -> read_char l, PLUS
      | '-' -> read_char l, MINUS
      | '*' -> read_char l, PRODUCT
      | '/' -> read_char l, DIVISION
      | '<' -> read_char l, LESS_THAN
      | '>' -> read_char l, GREATER_THAN
      | '=' ->
        (match peek_char l with
         | Some ch when Char.(ch = '=') -> read_char (read_char l), EQUALS
         | _ -> read_char l, ASSIGN)
      | '!' ->
        (match peek_char l with
         | Some ch when Char.(ch = '=') -> read_char (read_char l), NOT_EQUALS
         | _ -> read_char l, NOT)
      | '"' -> read_string l
      | ch when Char.is_alpha ch -> read_identifier l
      | ch when Char.is_digit ch -> read_integer l
      | _ -> l, ILLEGAL
    in
    l, Some token
;;

(* Tests *)
(* TODO: Improve testing and move to testing folder. Possibly with Alcotest or QuickCheck *)
let get_tokens input =
  let l = init input in
  let rec loop l acc =
    match next_token l with
    | _, None -> acc
    | l, Some token -> loop l (token :: acc)
  in
  loop l [] |> List.rev
;;

let%test "Test lexing" =
  let tokens =
    get_tokens
      {|
    let x = 69420 / 96;
    "this is a string";
    !(1 > 2) != (3 < 1);
    [1 * 2 + 1, 2 - 1];
    if (some == true) {
      return x
    } else {
      y
    }
  |}
  in
  List.iter ~f:(fun t -> Stdio.print_endline @@ Token.show t) tokens;
  tokens
  |> List.equal (fun a b -> Token.equal a b)
     @@
     let open Token in
     [ LET
     ; IDENT "x"
     ; ASSIGN
     ; INTEGER "69420"
     ; DIVISION
     ; INTEGER "96"
     ; SEMICOLON
     ; STRING "this is a string"
     ; SEMICOLON
     ; NOT
     ; LEFT_PAREN
     ; INTEGER "1"
     ; GREATER_THAN
     ; INTEGER "2"
     ; RIGHT_PAREN
     ; NOT_EQUALS
     ; LEFT_PAREN
     ; INTEGER "3"
     ; LESS_THAN
     ; INTEGER "1"
     ; RIGHT_PAREN
     ; SEMICOLON
     ; LEFT_BRACKET
     ; INTEGER "1"
     ; PRODUCT
     ; INTEGER "2"
     ; PLUS
     ; INTEGER "1"
     ; COMMA
     ; INTEGER "2"
     ; MINUS
     ; INTEGER "1"
     ; RIGHT_BRACKET
     ; SEMICOLON
     ; IF
     ; LEFT_PAREN
     ; IDENT "some"
     ; EQUALS
     ; TRUE
     ; RIGHT_PAREN
     ; LEFT_SQUIRLY
     ; RETURN
     ; IDENT "x"
     ; RIGHT_SQUIRLY
     ; ELSE
     ; LEFT_SQUIRLY
     ; IDENT "y"
     ; RIGHT_SQUIRLY
     ]
;;
