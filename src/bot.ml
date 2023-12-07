open Pieces
open Moves
open Board

let pawn_value = 100
let knight_value = 300
let bishop_value = 300
let rook_value = 500
let queen_value = 900

let generate_pawn start_pos last_move board turn lst : unit =
  let dir = if turn = White then -1 else 1 in
  let end_pos = (fst start_pos + dir, snd start_pos) in
  if
    within_bounds end_pos
    && check_pawn
         (piece_at_pos start_pos board)
         (piece_at_pos end_pos board)
         last_move board
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + (2 * dir), snd start_pos) in
  if
    within_bounds end_pos
    && check_pawn
         (piece_at_pos start_pos board)
         (piece_at_pos end_pos board)
         last_move board
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + dir, snd start_pos + 1) in
  if
    within_bounds end_pos
    && check_pawn
         (piece_at_pos start_pos board)
         (piece_at_pos end_pos board)
         last_move board
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + dir, snd start_pos - 1) in
  if
    within_bounds end_pos
    && check_pawn
         (piece_at_pos start_pos board)
         (piece_at_pos end_pos board)
         last_move board
  then lst := (start_pos, end_pos) :: !lst;
  ()

let generate_knight start_pos board turn lst : unit =
  let end_pos = (fst start_pos + 1, snd start_pos + 2) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + 1, snd start_pos - 2) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos - 1, snd start_pos + 2) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos - 1, snd start_pos - 2) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + 2, snd start_pos + 1) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos + 2, snd start_pos - 1) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos - 2, snd start_pos + 1) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  let end_pos = (fst start_pos - 2, snd start_pos - 1) in
  if
    within_bounds end_pos
    && check_knight (piece_at_pos start_pos board) (piece_at_pos end_pos board)
  then lst := (start_pos, end_pos) :: !lst;
  ()

let generate_king start_pos board turn lst : unit = let end_pos = (fst
   start_pos + 1, snd start_pos + 1) in if within_bounds end_pos && check_king
   (piece_at_pos start_pos board) (piece_at_pos end_pos board) board then lst :=
   (start_pos, end_pos) :: !lst; let end_pos = (fst start_pos + 1, snd
   start_pos) in if within_bounds end_pos && check_king (piece_at_pos start_pos
   board) (piece_at_pos end_pos board) board then lst := (start_pos, end_pos) ::
   !lst; let end_pos = (fst start_pos + 1, snd start_pos - 1) in if
   within_bounds end_pos && check_king (piece_at_pos start_pos board)
   (piece_at_pos end_pos board) board then lst := (start_pos, end_pos) :: !lst;
   let end_pos = (fst start_pos, snd start_pos + 1) in if within_bounds end_pos
   && check_king (piece_at_pos start_pos board) (piece_at_pos end_pos board)
   board then lst := (start_pos, end_pos) :: !lst; let end_pos = (fst start_pos,
   snd start_pos - 1) in if within_bounds end_pos && check_king (piece_at_pos
   start_pos board) (piece_at_pos end_pos board) board then lst := (start_pos,
   end_pos) :: !lst; let end_pos = (fst start_pos - 1, snd start_pos + 1) in if
   within_bounds end_pos && check_king (piece_at_pos start_pos board)
   (piece_at_pos end_pos board) board then lst := (start_pos, end_pos) :: !lst;
   let end_pos = (fst start_pos - 1, snd start_pos) in if within_bounds end_pos
   && check_king (piece_at_pos start_pos board) (piece_at_pos end_pos board)
   board then lst := (start_pos, end_pos) :: !lst;

let generate_moves board turn last_move : ((int * int) * (int * int)) list =
  let lst = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let p = piece_at_pos (i, j) board in
      match p.piece_type with
      | Pawn -> generate_pawn (i, j) last_move board turn lst
      | Knight -> generate_knight ()
      | Bishop -> generate_bishop ()
      | Rook -> generate_rook ()
      | Queen -> generate_queen ()
      | King -> generate_king ()
      | _ -> ()
    done
  done;
  !lst

let find_knights board turn : piece list =
  let lst = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let p = piece_at_pos (i, j) board in
      if p.piece_type = Knight && p.piece_color == turn then lst := p :: !lst
    done
  done;
  !lst

let generate_knight_moves board turn : unit =
  let myKnights = find_knights board turn in
  for i = 0 to List.length myKnights - 1 do
    print_string ""
  done;
  ()

let count_material (board : piece array array) (turn : color) : int =
  let score = ref 0 in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let p = piece_at_pos (i, j) board in
      match p.piece_type with
      | Pawn -> if p.piece_color = turn then score := !score + pawn_value
      | Knight -> if p.piece_color = turn then score := !score + knight_value
      | Bishop -> if p.piece_color = White then score := !score + bishop_value
      | Rook -> if p.piece_color = White then score := !score + rook_value
      | Queen -> if p.piece_color = White then score := !score + queen_value
      | _ -> ()
    done
  done;
  !score

let evaluate board turn =
  let white_eval = count_material board White in
  let black_eval = count_material board Black in
  let eval = white_eval - black_eval in
  if turn = White then eval else ~-eval

let rec search depth board turn king_loc alpha beta =
  if depth = 0 then evaluate board turn
  else
    (* TODO: fix move format and generate_moves *)
    let moves = generate_moves in
    if List.length moves = 0 then
      if under_check board turn king_loc then min_int else 0
    else
      let alpha = ref min_int in
      let i = ref 0 in
      let eval = ref min_int in
      while !i < List.length moves && !eval < beta do
        let move = List.nth moves !i in
        let _ = make_move move board turn in
        eval := ~-(search (depth - 1) board turn king_loc ~-beta ~- (!alpha));
        let _ = unmake_move board turn (fst move) (snd move) in
        i := !i + 1;
        alpha := max !alpha !eval
      done;
      if !eval >= beta then beta else !alpha
