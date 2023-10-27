open Chess
open Pieces
open Board

exception GameEnd
(** [GameEnd] is an exception.*)

let init_board = board

(* read-eval-print loop *)
let rec repl state : unit =
  print_endline "Enter a move. Format: <start pos> <end pos>";
  print_string "> ";
  let move = read_line () in
  match move with
  | "quit" -> print_endline "bye"
  (* add verification for checkmate, empassant, other moves, etc. *)
  | _ ->
      print_board (make_move move init_board);
      print_string "\n\n";
      repl state

(** black_winner is var of type [string]. It represents graphic displayed when
    black wins.*)

(* let () = print_board init_board *)

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Chess!";
  print_endline "Type 'checkmate' to quit.";
  print_endline "Type 'help' for a list of commands.";
  print_board board;
  repl true
