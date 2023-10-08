(** Lexer *)
type t

(** Initialize a lexer using a string as input *)
val init : string -> t

(** Consume a token if possible and return the new lexer.

    [let l = init "let x = 5" in
next_token l] returns [(newLexer, Some Token.LET)].
    The new lexer is still operating on the same input, but its read position is past the [LET] token*)
val next_token : t -> t * Token.t option
