open! Core
open Monkey_interpreter

let () =
  let rec loop () =
    printf "> ";
    Out_channel.flush Out_channel.stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> printf "End"
    | Some line ->
      (match line |> Lexer.init |> Parser.init |> Parser.parse with
       | Error error ->
         printf "%s\n" error;
         loop ()
       | Ok node ->
         print_endline (Eval.Object.show_obj (Eval.eval_node node));
         print_endline "";
         loop ())
  in
  loop ()
;;
