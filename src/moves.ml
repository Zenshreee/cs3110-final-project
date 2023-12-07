open Pieces

(* Dedup this later. Type also defined in board. *)
type board = piece array array

type last_move_type = {
  last_piece : piece;
  last_start_pos : int * int;
  last_end_pos : int * int;
}

(* If testing is true, alternating turns are not enforced. *)
let testing = false

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
  let col = int_of_char (String.get s 0) - int_of_char 'a' in
  let row = int_of_char (String.get s 1) - int_of_char '1' |> ( - ) 7 in
  (row, col)

(* 1. Check whether a position is within bounds of the game board. *)
let within_bounds (row, col) : bool =
  not (row < 0 || row > 7 || col < 0 || col > 7)

(* Helper: Determine the piece at a specific position based on the game
   board. *)
let piece_at_pos (pos : int * int) (board : board) : piece =
  let row, col = pos in
  let row = Array.get board row in
  let p = Array.get row col in
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

(* Check if opponent's pawn is threatening king *)
let pawn_checking board (row, col) opp =
  let dir = if opp = White then -1 else 1 in
  if col + dir < 0 || col + dir > 7 then false
  else
    (row - 1 >= 0
    &&
    let p = piece_at_pos (row - 1, col + dir) board in
    get_piece_color p = opp && get_piece_type p = Pawn)
    || row + 1 <= 7
       &&
       let p = piece_at_pos (row + 1, col + dir) board in
       get_piece_color p = opp && get_piece_type p = Pawn

(* Check if opponent's knight is threatening king *)
let knight_checking board (row, col) opp loc =
  let row', col' = (row + fst loc, col + snd loc) in
  row' >= 0 && col' >= 0 && row' <= 7 && col' <= 7
  &&
  let p = piece_at_pos (row', col') board in
  get_piece_color p = opp && get_piece_type p = Knight

(* Check if opponent's bishop, rook, or queen is threatening king *)
let rec check_line board (row, col) opp dir =
  let adjRow = row + fst dir in
  let adjCol = col + snd dir in
  if adjRow < 0 || adjRow > 7 || adjCol < 0 || adjCol > 7 then false
  else
    let p = piece_at_pos (adjRow, adjCol) board in
    if
      abs (fst dir) = abs (snd dir)
      && (get_piece_type p = Bishop || get_piece_type p = Queen)
    then get_piece_color p = opp
    else if
      abs (fst dir) <> abs (snd dir)
      && (get_piece_type p = Rook || get_piece_type p = Queen)
    then get_piece_color p = opp
    else if get_piece_type p = Blank then
      check_line board (adjRow, adjCol) opp dir
    else false

(* Check if opponent's king placed under check *)
let under_check board turn king_loc =
  let opp = if turn = Black then White else Black in
  let row, col = king_loc in
  pawn_checking board (row, col) opp
  || knight_checking board (row, col) opp (1, 2)
  || knight_checking board (row, col) opp (1, -2)
  || knight_checking board (row, col) opp (-1, 2)
  || knight_checking board (row, col) opp (-1, -2)
  || knight_checking board (row, col) opp (2, 1)
  || knight_checking board (row, col) opp (2, -1)
  || knight_checking board (row, col) opp (-2, 1)
  || knight_checking board (row, col) opp (-2, -1)
  || check_line board (row, col) opp (1, 1)
  || check_line board (row, col) opp (1, -1)
  || check_line board (row, col) opp (-1, 1)
  || check_line board (row, col) opp (-1, -1)
  || check_line board (row, col) opp (0, 1)
  || check_line board (row, col) opp (0, -1)
  || check_line board (row, col) opp (1, 0)
  || check_line board (row, col) opp (-1, 0)

(* Helper function to determine if path is clear for linear movements (used for
   bishop and rook). *)
let rec check_path (board : board) (step : int * int) (current_pos : int * int)
    (end_pos : int * int) atk_piece_color =
  let next_Row, next_Col =
    (fst current_pos + fst step, snd current_pos + snd step)
  in
  if next_Row = fst end_pos && next_Col = snd end_pos then
    board.(next_Row).(next_Col).piece_color <> atk_piece_color
  else
    board.(next_Row).(next_Col).piece_type = Blank
    && check_path board step (next_Row, next_Col) end_pos atk_piece_color

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
let check_pawn attacking_piece defending_piece (last_move : last_move_type)
    board =
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
  let row, col = atk_piece.piece_pos in
  let row', col' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    if
      (* All of knight's possible L-shaped moves *)
      (((row = row' + 2 || row = row' - 2) && (col = col' + 1 || col = col' - 1))
      || (row = row' + 1 || row = row' - 1)
         && (col = col' + 2 || col = col' - 2))
      && def_piece.piece_color <> atk_piece.piece_color
    then true
    else false
  end

(* 2. c) check valid move for King *)
let check_king atk_piece def_piece turn board king_moved krook_moved qrook_moved
    =
  let row, col = atk_piece.piece_pos in
  let row', col' = def_piece.piece_pos in
  if atk_piece.piece_pos = def_piece.piece_pos then false
  else begin
    let king_side =
      col = 4 && col' = 6
      && ((turn = White && row = 7 && row' = 7)
         || (turn = Black && row = 0 && row' = 0))
      && (not king_moved) && not krook_moved
    in
    let queen_side =
      col = 4 && col' = 2
      && ((turn = White && row = 7 && row' = 7)
         || (turn = Black && row = 0 && row' = 0))
      && (not king_moved) && not qrook_moved
    in
    if
      (* All of king's possible one-step moves *)
      (row = row' + 1 || row = row' - 1 || row = row')
      && (col = col' + 1 || col = col' - 1 || col = col')
      && def_piece.piece_color <> atk_piece.piece_color
    then true
    else if king_side then
      (piece_at_pos (row, col + 1) board).piece_type = Blank
      && (piece_at_pos (row, col + 2) board).piece_type = Blank
      && not
           (under_check board turn (row, 4)
           || under_check board turn (row, 5)
           || under_check board turn (row, 6))
    else if queen_side then
      (piece_at_pos (row, col - 1) board).piece_type = Blank
      && (piece_at_pos (row, col - 2) board).piece_type = Blank
      && (piece_at_pos (row, col - 3) board).piece_type = Blank
      && not
           (under_check board turn (row, 4)
           || under_check board turn (row, 3)
           || under_check board turn (row, 2)
           || under_check board turn (row, 1))
    else false
  end

(* 2. d) check valid move for Rook *)
let check_rook (board : board) atk_piece def_piece =
  let row, col = atk_piece.piece_pos in
  let row', col' = def_piece.piece_pos in

  if row = row' || col = col' then
    let step =
      if row = row' then if col' > col then (0, 1) else (0, -1)
      else if row' > row then (1, 0)
      else (-1, 0)
    in
    check_path board step (row, col) (row', col') atk_piece.piece_color
  else false

(* 2. e) check valid move for Bishop *)
let check_bishop (board : board) atk_piece def_piece =
  let row, col = atk_piece.piece_pos in
  let row', col' = def_piece.piece_pos in

  let row_diff, col_diff = (abs (row - row'), abs (col - col')) in
  if row_diff = col_diff then
    let step = ((row' - row) / row_diff, (col' - col) / col_diff) in
    check_path board step (row, col) (row', col') atk_piece.piece_color
  else false

(* 2. e) check valid move for Queen *)
let check_queen board atk_piece def_piece =
  check_rook board atk_piece def_piece || check_bishop board atk_piece def_piece

(* 3. Check whether a move is valid for a given piece *)
let valid_move (board : board) (atk_piece : piece) (move : int * int)
    (turn : color) (last_move : last_move_type) (king_moved : bool)
    (krook_moved : bool) (qrook_moved : bool) : bool =
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
    | King ->
        check_king atk_piece def_piece turn board king_moved krook_moved
          qrook_moved
    | Rook -> check_rook board atk_piece def_piece
    | Bishop -> check_bishop board atk_piece def_piece
    | Queen -> check_queen board atk_piece def_piece
