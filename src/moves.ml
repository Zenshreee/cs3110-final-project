open Pieces

(* Dedup this later. Type also defined in board. *)
type board = piece array array

type last_move = {
  last_piece : piece;
  last_start_pos : int * int;
  last_end_pos : int * int;
}

(* If testing is true, alternating turns are not enforced. *)
let testing = true

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
let piece_at_pos (pos : int * int) (board : board) : piece =
  let x, y = pos in
  let row = Array.get board x in
  let p = Array.get row y in
  p

(* 2. Check if the pawn is moving to a valid square. atk_piece indicates the
   starting position, while def_piece indicates the ending position. *)

(* Debugging helper for printing the color of a piece. *)
let string_of_piece_type piece_type =
  match piece_type with
  | Pawn -> "Pawn"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | King -> "King"
  | Blank -> "Blank"

(* Helper function to determine if path is clear for linear movements (used for
   bishop and rook). *)
let rec check_path (board : board) (step : int * int)
    (current_pos : int * int) (end_pos : int * int) atk_piece_color =
  let next_x, next_y =
    (fst current_pos + fst step, snd current_pos + snd step)
  in
  if next_x = fst end_pos && next_y = snd end_pos then
    board.(next_x).(next_y).piece_color <> atk_piece_color
  else
    board.(next_x).(next_y).piece_type = Blank
    && check_path board step (next_x, next_y) end_pos atk_piece_color

(* Helper function to check if move is en_passant move. *)
let is_en_passant_move attacking_pawn last_move end_pos board =
  let start_row, start_col = attacking_pawn.piece_pos in
  let end_row, end_col = end_pos in
  let last_start_row, last_start_col = last_move.last_start_pos in
  let last_end_row, last_end_col = last_move.last_end_pos in

  last_move.last_piece.piece_type = Pawn
  && abs (last_start_row - last_end_row) = 2
  && abs (start_col - end_col) = 1
  && start_row = last_end_row
  && abs (end_row - last_end_row) = 1
  && board.(last_end_row).(last_end_col).piece_type = Pawn
  && ((attacking_pawn.piece_color = White && start_row = 3 && end_row = 2)
     || (attacking_pawn.piece_color = Black && start_row = 4 && end_row = 5))
  && board.(last_end_row).(last_end_col).piece_color
     <> attacking_pawn.piece_color

(* 2. a) check valid move for Pawn *)
let check_pawn attacking_piece defending_piece (last_move : last_move) board =
  let start_row, start_col = attacking_piece.piece_pos in
  let target_row, target_col = defending_piece.piece_pos in
  let color = attacking_piece.piece_color in
  let direction = target_row - start_row in

  (* Determine direction based on row difference *)
  if direction > 0 && color <> Black then false
  else if direction < 0 && color <> White then false
  else if attacking_piece.piece_pos = defending_piece.piece_pos then false
  else
    match direction with
    | (1 | -1)
      when is_en_passant_move attacking_piece last_move
             defending_piece.piece_pos board -> true
    | 1 | -1 ->
        (* Single step forward *)
        (target_col = start_col && defending_piece.piece_type = Blank)
        || abs (target_col - start_col) = 1
           && defending_piece.piece_color <> None
           && defending_piece.piece_color <> attacking_piece.piece_color
    | 2 when start_row = 1 && target_col = start_col ->
        (* Black pawn's initial two-step move *)
        defending_piece.piece_type = Blank
    | -2 when start_row = 6 && target_col = start_col ->
        (* White pawn's initial two-step move *)
        defending_piece.piece_type = Blank
    | _ -> false

(* 2. b) check valid move for Knight *)
let check_knight atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    if
      (* All of knight's possible L-shaped moves *)
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
      (* All of king's possible one-step moves *)
      (x = x' + 1 || x = x' - 1 || x = x')
      && (y = y' + 1 || y = y' - 1 || y = y')
      && def_piece.piece_color <> atk_piece.piece_color
    then true
    else false
  end

(* 2. d) check valid move for Rook *)
let check_rook (board : board) atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in

  if x = x' || y = y' then
    let step =
      if x = x' then if y' > y then (0, 1) else (0, -1)
      else if x' > x then (1, 0)
      else (-1, 0)
    in
    check_path board step (x, y) (x', y') atk_piece.piece_color
  else false

(* 2. e) check valid move for Bishop *)
let check_bishop (board : board) atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in

  let x_diff, y_diff = (abs (x - x'), abs (y - y')) in
  if x_diff = y_diff then
    let step = ((x' - x) / x_diff, (y' - y) / y_diff) in
    check_path board step (x, y) (x', y') atk_piece.piece_color
  else false

(* 2. e) check valid move for Queen *)
let check_queen board atk_piece def_piece =
  check_rook board atk_piece def_piece || check_bishop board atk_piece def_piece

(* Check if opponent's pawn is threatening king *)
let pawn_checking board (x, y) opp =
  let dir = if opp = White then -1 else 1 in
  if y + dir < 0 || y + dir > 7 then false
  else
    (x - 1 >= 0
    &&
    let p = piece_at_pos (x - 1, y + dir) board in
    get_piece_color p = opp && get_piece_type p = Pawn)
    || x + 1 <= 7
       &&
       let p = piece_at_pos (x + 1, y + dir) board in
       get_piece_color p = opp && get_piece_type p = Pawn

(* Check if opponent's knight is threatening king *)
let knight_checking board (x, y) opp loc =
  let x', y' = (x + fst loc, y + snd loc) in
  x' >= 0 && y' >= 0 && x' <= 7 && y' <= 7
  &&
  let p = piece_at_pos (x', y') board in
  get_piece_color p = opp && get_piece_type p = Knight

(* Check if opponent's bishop, rook, or queen is threatening king *)
let rec check_line board (x, y) opp dir =
  let adjx = x + fst dir in
  let adjy = y + snd dir in
  if adjx < 0 || adjx > 7 || adjy < 0 || adjy > 7 then false
  else
    let p = piece_at_pos (adjx, adjy) board in
    if
      abs (fst dir) = abs (snd dir)
      && (get_piece_type p = Bishop || get_piece_type p = Queen)
    then get_piece_color p = opp
    else if
      abs (fst dir) <> abs (snd dir)
      && (get_piece_type p = Rook || get_piece_type p = Queen)
    then get_piece_color p = opp
    else if get_piece_type p = Blank then check_line board (adjx, adjy) opp dir
    else false

(* Check if opponent's king placed under check *)
let under_check board turn king_loc =
  let opp = if turn = Black then White else Black in
  let x, y = if turn = White then fst king_loc else snd king_loc in
  pawn_checking board (x, y) opp
  || knight_checking board (x, y) opp (1, 2)
  || knight_checking board (x, y) opp (1, -2)
  || knight_checking board (x, y) opp (-1, 2)
  || knight_checking board (x, y) opp (-1, -2)
  || knight_checking board (x, y) opp (2, 1)
  || knight_checking board (x, y) opp (2, -1)
  || knight_checking board (x, y) opp (-2, 1)
  || knight_checking board (x, y) opp (-2, -1)
  || check_line board (x, y) opp (1, 1)
  || check_line board (x, y) opp (1, -1)
  || check_line board (x, y) opp (-1, 1)
  || check_line board (x, y) opp (-1, -1)
  || check_line board (x, y) opp (0, 1)
  || check_line board (x, y) opp (0, -1)
  || check_line board (x, y) opp (1, 0)
  || check_line board (x, y) opp (-1, 0)

(* 3. Check whether a move is valid for a given piece *)
let valid_move (board : board) (atk_piece : piece)
    (move : int * int) (turn : color) (last_move : last_move) : bool =
  let check_turn_color atk_piece turn : bool =
    if atk_piece.piece_color = turn then true else false
  in

  if (not (check_turn_color atk_piece turn)) && not testing then false
  else
    let def_piece = piece_at_pos move board in
    match atk_piece.piece_type with
    | Blank -> false
    | Pawn -> check_pawn atk_piece def_piece last_move board
    | Knight -> check_knight atk_piece def_piece
    | King -> check_king atk_piece def_piece
    | Rook -> check_rook board atk_piece def_piece
    | Bishop -> check_bishop board atk_piece def_piece
    | Queen -> check_queen board atk_piece def_piece
