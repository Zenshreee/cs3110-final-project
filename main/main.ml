open Chess
open Pieces
open Moves
open Board



exception GameEnd
(** [GameEnd] is an exception.*)

let init_board = board
let curr_board = Array.copy board

(* read-eval-print loop *)
let rec repl state : unit =
  print_endline "Enter a legal move. Format: <start pos> <end pos>";
  (match state with
  | White -> print_endline "White's turn"
  | Black -> print_endline "Black's turn"
  | _ -> print_endline "");
  print_string "> ";
  let move = read_line () in
  match move with
  | "quit" -> print_endline "bye"
  | _ -> (
      let b, valid = make_move move curr_board state in
      match valid with
      | true ->
          print_board b;
          print_string "\n\n";
          let color =
            match state with
            | White -> Black
            | Black -> White
            | _ -> None
          in
          repl color
      | false ->
          print_board b;
          print_string "\n\n";
          repl state)

(** black_winner is var of type [string]. It represents graphic displayed when
    black wins.*)

(* let () = print_board init_board *)

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Chess!";
  print_endline "Type 'quit' to quit.";
  (* print_endline "Type 'help' for a list of commands."; *)
  print_board board;
  repl White
