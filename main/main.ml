open Chess
open Pieces
open Moves
open Board

(* [GameEnd] is an exception.*)
exception GameEnd

let curr_board = board

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
  (* add verification for checkmate, empassant, other moves, etc. *)
  (* | "reset" -> let curr_board = Array.copy board in print_board curr_board;
     print_string "\n\n"; let color = match state with | White -> Black | Black
     -> White | _ -> None in repl color *)
  | _ -> (
      let valid = make_move move curr_board state in
      match valid with
      | true ->
          print_board curr_board;
          print_string "\n\n";
          let color =
            match state with
            | White -> Black
            | Black -> White
            | _ -> None
          in
          repl color
      | false ->
          print_board curr_board;
          print_string "\n\n";
          repl state)

(** black_winner is var of type [string]. It represents graphic displayed when
    black wins.*)

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Chess!";
  print_endline "Type 'quit' to quit.";
  (* print_endline "Type 'help' for a list of commands."; *)
  print_board board;
  repl White
