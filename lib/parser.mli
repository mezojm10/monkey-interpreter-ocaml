type t

val init : Lexer.t -> t
val parse : t -> (Ast.node, string) result

type precedence =
  ([ `LOWEST
   | `EQUALS
   | `LESSGREATER
   | `SUM
   | `PRODUCT
   | `PREFIX
   | `CALL
   ]
  [@deriving show, ord])
(* val errors : t -> string list *)
