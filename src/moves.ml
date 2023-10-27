open Pieces

(* Given the chosen piece, suggested move, and game board state, we must verify
   that the move is a valid one. There are three checks that must be made: (1)
   Check if the move is within the bounds of the game board. (2) Check if the
   move is a valid move for the chosen piece. (3) Check if the move is a valid
   move for the chosen piece given the current state of the game board. *)

(* 0. parsing function to extract piece position from a move instruction. *)
let position_of_string : string -> int * int =
 fun s ->
  let x = int_of_char (String.get s 0) - int_of_char 'a' in
  let y = int_of_char (String.get s 1) - int_of_char '1' in
  (y, x)

(* 1. Check whether a position is within bounds of the game board. *)
let within_bounds (x, y) : bool =
  if x < 0 || x > 7 || y < 0 || y > 7 then false else true

(* Helper: Determine the piece at a specific position based on the game
   board. *)
let piece_at_pos (pos : int * int) (board : piece array array) : piece =
  let x, y = pos in
  let row = Array.get board x in
  let p = Array.get row y in
  p

(* 2. Check whether a move is valid for a given piece *)
let valid_move piece move : bool =
  let x, y = position_of_string move in
  match piece with
  | Pawn -> if x = 0 then false else true
  | _ -> false

(* 3. Check whether a move is valid for a given piece given the current state of
   the game board *)

let verify_move board piece move : bool = failwith "unimplemented"

let move_piece board piece move : 'a =
  let verify_move_placeholder = true in
  assert verify_move_placeholder
