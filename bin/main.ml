open! Core
open Monkey_interpreter

let () =
  let rec loop env =
    printf "> ";
    Out_channel.flush Out_channel.stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> printf "End"
    | Some line ->
      (match line |> Lexer.init |> Parser.init |> Parser.parse with
       | Error error ->
         printf "%s\n" error;
         loop env
       | Ok node ->
         let obj, env = Eval.eval_node node env in
         print_endline (Eval.Object.show obj);
         print_endline "";
         loop env)
  in
  loop Eval.Environment.init
;;
