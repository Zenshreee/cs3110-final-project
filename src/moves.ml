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

(* Helper: Determine the color of a piece. *)
let string_of_piece_type piece_type =
  match piece_type with
  | Pawn -> "Pawn"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | King -> "King"
  | Blank -> "Blank"

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

(* 2. d) check valid move for Rook *)
(* let check_rook (board : piece array array) atk_piece def_piece =
  let result = ref true in
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if x = x' then
    print_endline "x = x'";
    if y - y' < 0 then
      for i = y to y' - 1 do
        if board.(x).(i).piece_type <> Blank then 
          print_endline "1";
          result := false
      done
    else if y - y' > 0 then
      for i = y downto y' + 1 do
        if board.(x).(i).piece_type <> Blank then 
          print_endline "2";
          result := false
      done
  else if y = y' then
    print_endline "y = y'";
    print_endline (string_of_bool !result);
    if x - x' < 0 then
      for i = x + 1 to x' - 1 do
        if board.(i).(y).piece_type <> Blank then 
          print_endline "3";
        result := false;
        print_endline "hi";
        print_endline (string_of_bool !result);
      done
    else if x - x' > 0 then
      print_endline "hbflihe";
      print_endline (string_of_bool !result);
      for i = x - 1 downto x' + 1 do
        print_endline(string_of_piece_type board.(i).(y).piece_type);
        if (board.(i).(y).piece_type <> Blank) then 
          print_endline "4";
          result := false
      done;
  print_endline "bye";
  print_endline(string_of_bool !result);
  !result && def_piece.piece_color <> atk_piece.piece_color *)

(* let check_bishop (board : piece array array) atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  let slope = (y' - y) / (x' - x) in
  if slope <> 1 || slope <> -1 then false
  else begin
    let result = ref true in
    if slope = 1 then
      if x - x' < 0 then
        for i = x to x' - 1 do
          if board.(i).(i).piece_type <> Blank then result := false
        done
      else
        for i = x downto x' + 1 do
          if board.(i).(i).piece_type <> Blank then result := false
        done
    else if slope = -1 then
      if x - x' < 0 then
        for i = x to x' - 1 do
          if board.(i).(y - i).piece_type <> Blank then result := false
        done
      else
        for i = x downto x' + 1 do
          if board.(i).(y - i).piece_type <> Blank then result := false
        done;
    !result && def_piece.piece_color <> atk_piece.piece_color
  end *)

(* 2. e) check valid move for Queen *)
(* let check_queen board atk_piece def_piece =
  let x, y = atk_piece.piece_pos in
  let x', y' = def_piece.piece_pos in
  if (x = x' || y = y') && not (x = x' && y = y') then
    check_rook board atk_piece def_piece
  else check_bishop board atk_piece def_piece *)

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
    (* | Rook -> check_rook board atk_piece def_piece
    | Bishop -> check_bishop board atk_piece def_piece
    | Queen -> check_queen board atk_piece def_piece *)
    | _ -> false

(* 4. Check whether a move is valid for a given piece given the current state of
   the game board. *)

(* let verify_move (board : piece array array) (piece : piece) (move : int *
   int) : bool = let valid = valid_move board piece move in valid *)

let move_piece board piece move : 'a =
  let verify_move_placeholder = true in
  assert verify_move_placeholder
