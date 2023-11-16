open Pieces

(* Given the chosen piece, suggested move, and game board state, we must verify
   that the move is a valid one. There are three checks that must be made: (1)
   Check if the move is within the bounds of the game board. (2) Check if the
   move is a valid move for the chosen piece. (3) Check if the move is a valid
   move for the chosen piece given the current state of the game board. *)

(** 0. parsing function to extract piece position from a move instruction.
    Input: string of length 2, where the first character is a letter from a-h,
    and the second character is a number from 1-8. E.g., 'a2'. Returns int_1,
    int_2 where int_1 is row number (of the board array), and int_2 is column
    number (of the board array). *)
let position_of_string : string -> int * int =
 fun s ->
  let x = int_of_char (String.get s 0) - int_of_char 'a' in
  let y = int_of_char (String.get s 1) - int_of_char '1' |> ( - ) 7 in
  (y, x)

(* 1. Check whether a position is within bounds of the game board. *)
let within_bounds (x, y) : bool = not (x < 0 || x > 7 || y < 0 || y > 7)

(* Helper: Determine the piece at a specific position based on the game
   board. *)
let piece_at_pos (pos : int * int) (board : piece array array) : piece =
  let x, y = pos in
  let row = Array.get board x in
  let p = Array.get row y in
  p

(* 2. Check if the pawn is moving to a valid square. atk_piece indicates the
   starting position, while def_piece indicates the ending position. *)

(* 2. a) check valid move for Pawn *)
let check_pawn atk_piece def_piece dir =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    if x + (2 * dir) = x' then (
      print_endline "pawn moves 2";
      if dir = 1 && x = 1 && def_piece.piece_type = Blank then true
      else if dir = -1 && x = 6 && def_piece.piece_type = Blank then true
      else false)
    else if x + dir = x' then
      if y = y' && def_piece.piece_type = Blank then true
      else if
        (y = y' + 1 || y = y' - 1)
        && def_piece.piece_color <> None
        && def_piece.piece_color <> atk_piece.piece_color
      then true
      else false
    else false
  end

(* 2. b) check valid move for Knight *)
let check_knight atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    if
      (((x = x' + 2 || x = x' - 2) && (y = y' + 1 || y = y' - 1))
      || ((x = x' + 1 || x = x' - 1) && (y = y' + 2 || y = y' - 2)))
      && def_piece.piece_color <> atk_piece.piece_color
    then true
    else false
  end

(* 2. c) check valid move for King *)
let check_king atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    if
      (x = x' + 1 || x = x' - 1 || x = x')
      && (y = y' + 1 || y = y' - 1 || y = y')
      && def_piece.piece_color <> atk_piece.piece_color
    then true
    else false
  end

let check_path 

(* 2. d) check valid move for Rook *)
let check_rook atk_piece def_piece = 
  let x, y = atk_piece.piece_pos in let x', y' = def_piece.piece_pos in
  if (x = x')

let check_bishop atk_piece def_piece = 
  let x, y = atk_piece.piece_pos in let x', y' = def_piece.piece_pos in
  let slope = (y' - y) / (x' - x) in if slope <> 1 || slope <> -1 then false else 
    

(* 2. e) check valid move for Queen *)



let check_color atk_piece turn : bool =
  if atk_piece.piece_color = turn then true else false

(* 3. Check whether a move is valid for a gven piece *)
let valid_move board atk_piece move turn : bool =
  if not (check_color atk_piece turn) then false
  else
    let def_piece = piece_at_pos move board in
    match atk_piece.piece_type with
    | Blank -> false
    | Pawn ->
        if get_piece_colour atk_piece = Black then
          check_pawn atk_piece def_piece
            (let a = 1 in
             a)
        else
          check_pawn atk_piece def_piece
            (let a = -1 in
             a)
    | Knight -> check_knight atk_piece def_piece
    | King -> check_king atk_piece def_piece
    | _ -> true

(* 4. Check whether a move is valid for a given piece given the current state of
   the game board. *)

(* let verify_move (board : piece array array) (piece : piece) (move : int *
   int) : bool = let valid = valid_move board piece move in valid *)

let move_piece board piece move : 'a =
  let verify_move_placeholder = true in
  assert verify_move_placeholder
