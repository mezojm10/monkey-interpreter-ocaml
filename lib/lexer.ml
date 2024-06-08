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
    { l with ch; read_position = l.read_position + 1 })
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
  l, Token.Integer int_lit
;;

(** Just keeps reading until the terminating ['"'] or reaching EOF *)
let rec read_string_aux l aux =
  match l.ch with
  | None -> l, None
  | Some c when Char.(c <> '"') -> aux ^ Char.to_string c |> read_string_aux (read_char l)
  | Some _ -> l, Some (Token.String aux)
;;

let read_string l =
  let l = read_char l in
  match read_string_aux l "" with
  (* If None was returned, that means we reached EOF without the string being terminated *)
  | l, None -> l, Token.Illegal
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
      | '[' -> read_char l, Left_bracket
      | ']' -> read_char l, Right_bracket
      | '(' -> read_char l, Left_paren
      | ')' -> read_char l, Right_paren
      | '{' -> read_char l, Left_squirly
      | '}' -> read_char l, Right_squirly
      | ':' -> read_char l, Colon
      | ';' -> read_char l, Semicolon
      | ',' -> read_char l, Comma
      | '+' -> read_char l, Plus
      | '-' -> read_char l, Minus
      | '*' -> read_char l, Product
      | '/' -> read_char l, Division
      | '<' -> read_char l, Less_than
      | '>' -> read_char l, Greater_than
      | '=' ->
        (match peek_char l with
         | Some ch when Char.(ch = '=') -> read_char (read_char l), Equals
         | _ -> read_char l, Assign)
      | '!' ->
        (match peek_char l with
         | Some ch when Char.(ch = '=') -> read_char (read_char l), Not_equals
         | _ -> read_char l, Not)
      | '"' -> read_string l
      | ch when Char.is_alpha ch -> read_identifier l
      | ch when Char.is_digit ch -> read_integer l
      | _ -> l, Illegal
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
  tokens
  |> List.equal (fun a b -> Token.equal a b)
     @@
     let open Token in
     [ Let
     ; Ident "x"
     ; Assign
     ; Integer "69420"
     ; Division
     ; Integer "96"
     ; Semicolon
     ; String "this is a string"
     ; Semicolon
     ; Not
     ; Left_paren
     ; Integer "1"
     ; Greater_than
     ; Integer "2"
     ; Right_paren
     ; Not_equals
     ; Left_paren
     ; Integer "3"
     ; Less_than
     ; Integer "1"
     ; Right_paren
     ; Semicolon
     ; Left_bracket
     ; Integer "1"
     ; Product
     ; Integer "2"
     ; Plus
     ; Integer "1"
     ; Comma
     ; Integer "2"
     ; Minus
     ; Integer "1"
     ; Right_bracket
     ; Semicolon
     ; If
     ; Left_paren
     ; Ident "some"
     ; Equals
     ; True
     ; Right_paren
     ; Left_squirly
     ; Return
     ; Ident "x"
     ; Right_squirly
     ; Else
     ; Left_squirly
     ; Ident "y"
     ; Right_squirly
     ]
;;
