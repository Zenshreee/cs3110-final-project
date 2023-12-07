open Pieces
open Moves

(************************ Initialize pieces and board. ************************)

type white_pieces = {
  king : string;
  queen : string;
  rook : string;
  knight : string;
  bishop : string;
  pawn : string;
}

let white_pieces =
  {
    king = "♔";
    queen = "♕";
    rook = "♖";
    knight = "♘";
    bishop = "♗";
    pawn = "♙";
  }

type black_pieces = {
  king : string;
  queen : string;
  rook : string;
  knight : string;
  bishop : string;
  pawn : string;
}

let black_pieces =
  {
    king = "♚";
    queen = "♛";
    rook = "♜";
    knight = "♞";
    bishop = "♝";
    pawn = "♟︎";
  }

type board = piece array array

let board =
  [|
    [|
      make_piece Rook Black (0, 0);
      make_piece Knight Black (0, 1);
      make_piece Bishop Black (0, 2);
      make_piece Queen Black (0, 3);
      make_piece King Black (0, 4);
      make_piece Bishop Black (0, 5);
      make_piece Knight Black (0, 6);
      make_piece Rook Black (0, 7);
    |];
    [|
      make_piece Pawn Black (1, 0);
      make_piece Pawn Black (1, 1);
      make_piece Pawn Black (1, 2);
      make_piece Pawn Black (1, 3);
      make_piece Pawn Black (1, 4);
      make_piece Pawn Black (1, 5);
      make_piece Pawn Black (1, 6);
      make_piece Pawn Black (1, 7);
    |];
    [|
      make_piece Blank None (2, 0);
      make_piece Blank None (2, 1);
      make_piece Blank None (2, 2);
      make_piece Blank None (2, 3);
      make_piece Blank None (2, 4);
      make_piece Blank None (2, 5);
      make_piece Blank None (2, 6);
      make_piece Blank None (2, 7);
    |];
    [|
      make_piece Blank None (3, 0);
      make_piece Blank None (3, 1);
      make_piece Blank None (3, 2);
      make_piece Blank None (3, 3);
      make_piece Blank None (3, 4);
      make_piece Blank None (3, 5);
      make_piece Blank None (3, 6);
      make_piece Blank None (3, 7);
    |];
    [|
      make_piece Blank None (4, 0);
      make_piece Blank None (4, 1);
      make_piece Blank None (4, 2);
      make_piece Blank None (4, 3);
      make_piece Blank None (4, 4);
      make_piece Blank None (4, 5);
      make_piece Blank None (4, 6);
      make_piece Blank None (4, 7);
    |];
    [|
      make_piece Blank None (5, 0);
      make_piece Blank None (5, 1);
      make_piece Blank None (5, 2);
      make_piece Blank None (5, 3);
      make_piece Blank None (5, 4);
      make_piece Blank None (5, 5);
      make_piece Blank None (5, 6);
      make_piece Blank None (5, 7);
    |];
    [|
      make_piece Pawn White (6, 0);
      make_piece Pawn White (6, 1);
      make_piece Pawn White (6, 2);
      make_piece Pawn White (6, 3);
      make_piece Pawn White (6, 4);
      make_piece Pawn White (6, 5);
      make_piece Pawn White (6, 6);
      make_piece Pawn White (6, 7);
    |];
    [|
      make_piece Rook White (7, 0);
      make_piece Knight White (7, 1);
      make_piece Bishop White (7, 2);
      make_piece Queen White (7, 3);
      make_piece King White (7, 4);
      make_piece Bishop White (7, 5);
      make_piece Knight White (7, 6);
      make_piece Rook White (7, 7);
    |];
  |]

(* Initial king placement *)
let king_loc = ref ((7, 4), (0, 4))
let king_moved = ref (false, false)
let krook_moved = ref (false, false)
let qrook_moved = ref (false, false)

(**************************** End initialization. *****************************)

(* Keep track of the last move made on the board. *)
let last_move : last_move ref =
  ref
    {
      last_piece =
        { piece_type = Blank; piece_color = White; piece_pos = (0, 0) };
      last_start_pos = (0, 0);
      last_end_pos = (0, 0);
    }

(* Helper function to print the board. *)
let print_board board =
  for i = 0 to 7 do
    print_string "\n------------------------------------\n";
    print_string (" " ^ string_of_int (8 - i));
    for j = 0 to 7 do
      print_string " | ";
      match board.(i).(j) with
      | { piece_type = Blank; piece_color = _; piece_pos = _ } ->
          print_string " "
      | { piece_type = p; piece_color = Black; piece_pos = _ } -> (
          match p with
          | Pawn -> print_string black_pieces.pawn
          | Rook -> print_string black_pieces.rook
          | Knight -> print_string black_pieces.knight
          | Bishop -> print_string black_pieces.bishop
          | Queen -> print_string black_pieces.queen
          | King -> print_string black_pieces.king
          | _ -> print_string " |")
      | { piece_type = p; piece_color = White; piece_pos = _ } -> (
          match p with
          | Pawn -> print_string white_pieces.pawn
          | Rook -> print_string white_pieces.rook
          | Knight -> print_string white_pieces.knight
          | Bishop -> print_string white_pieces.bishop
          | Queen -> print_string white_pieces.queen
          | King -> print_string white_pieces.king
          | _ -> print_string " |")
      | _ -> print_string " "
    done;
    print_string " | "
  done;
  print_string
    "\n\
     ------------------------------------\n\
    \   | a | b | c | d | e | f | g | h |\n"

(* Helper function to print a tuple. *)
let print_tuple (row, col) =
  print_string "(";
  print_int row;
  print_string ", ";
  print_int col;
  print_string ")"

(* Sets a piece on board curr at position pos. *)
let board_set piece pos curr =
  let z, w = pos in
  let row = curr.(z) in
  let p = set_piece_pos piece pos in
  row.(w) <- p;
  curr.(z) <- row

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
  && ((attacking_pawn.piece_color = White && start_row = 3 && end_row = 2)
     || (attacking_pawn.piece_color = Black && start_row = 4 && end_row = 5))
  && board.(last_end_row).(last_end_col).piece_color
     <> attacking_pawn.piece_color

(* Helper function to check if move is a pawn promotion. *)
let is_pawn_promotion attacking_pawn end_pos board =
  let is_pawn = attacking_pawn.piece_type = Pawn in
  let end_row, _ = end_pos in
  let color = attacking_pawn.piece_color in
  match color with
  | White when is_pawn -> end_row = 0
  | Black when is_pawn -> end_row = 7
  | _ -> false

(* Helper function to update the board and last move. *)
let update_board_and_last_move p start_pos end_pos new_board =
  last_move :=
    { last_piece = p; last_start_pos = start_pos; last_end_pos = end_pos };
  board_set p end_pos new_board

(* Helper function to determine allotted promotion piece. *)
let rec ask_match_choice _ =
  print_string "\nEnter a valid piece type to promote to: ";
  let choice = read_line () in
  let lower = String.lowercase_ascii choice in
  match lower with
  | "knight" -> Knight
  | "queen" -> Queen
  | "bishop" -> Bishop
  | "rook" -> Rook
  | _ -> ask_match_choice ()

(* Precondition: Input must be in chess notation. For example "e4 e5". *)

let make_move (move : string) (curr_game_state : piece array array)
    (turn : color) : board * bool =
  (* Parse the given move. *)
  let start_pos =
    try position_of_string (String.sub move 0 2)
    with Invalid_argument e -> (-1, -1)
  in

  let end_pos =
    try position_of_string (String.sub move 3 2)
    with Invalid_argument e -> (-1, -1)
  in

  if
    start_pos <> (-1, -1)
    && end_pos <> (-1, -1)
    && within_bounds end_pos && within_bounds start_pos
  then
    let p = piece_at_pos start_pos curr_game_state in
    let k_move = if turn = White then fst !king_moved else snd !king_moved in
    let kr_move = if turn = White then fst !krook_moved else snd !krook_moved in
    let qr_move = if turn = White then fst !qrook_moved else snd !qrook_moved in
    if
      valid_move curr_game_state p end_pos turn !last_move k_move kr_move
        qr_move
    then (
      (* Set the start position to blank. *)
      let prev = !last_move in
      board_set (make_piece Blank None start_pos) start_pos curr_game_state;
      if
        (* Check if move is en passant move. *)
        is_en_passant_move p !last_move end_pos curr_game_state
      then begin
        print_string "En passant move occurred\n";
        let end_row, end_col = end_pos in
        let captured_pawn_row =
          if p.piece_color = White then end_row + 1 else end_row - 1
        in
        let captured_pawn =
          make_piece Blank None (captured_pawn_row, end_col)
        in
        board_set captured_pawn (captured_pawn_row, end_col) curr_game_state;
        update_board_and_last_move p start_pos end_pos curr_game_state
      end
      else if is_pawn_promotion p end_pos board then begin
        board_set
          (make_piece p.piece_type p.piece_color end_pos)
          end_pos curr_game_state;
        print_string
          "Pawn promotion occurred\nHere is the current game state.\n";
        print_board board;
        let piece_type = ask_match_choice () in
        let color = p.piece_color in
        update_board_and_last_move
          (make_piece piece_type color end_pos)
          start_pos end_pos curr_game_state
      end
      else update_board_and_last_move p start_pos end_pos curr_game_state;

      king_loc :=
        if get_piece_type p = King then
          if get_piece_color p = White then (end_pos, snd !king_loc)
          else (fst !king_loc, end_pos)
        else !king_loc;

      if
        under_check curr_game_state
          (if turn = White then Black else White)
          (if turn = White then snd !king_loc else fst !king_loc)
      then print_endline "\nCHECK";

      if
        under_check curr_game_state turn
          (if turn = White then fst !king_loc else snd !king_loc)
      then (
        last_move := prev;
        print_endline "Under check. Try again.";
        board_set (make_piece Blank None end_pos) end_pos curr_game_state;
        board_set p start_pos curr_game_state;
        (curr_game_state, false))
      else (
        print_string "Valid move made\n";
        king_moved :=
          ( (piece_at_pos (7, 4) board).piece_type <> King || fst !king_moved,
            (piece_at_pos (0, 4) board).piece_type <> King || snd !king_moved );
        krook_moved :=
          ( (let p = piece_at_pos (7, 7) board in
             p.piece_type <> Rook || p.piece_color <> White)
            || fst !krook_moved,
            (let p = piece_at_pos (0, 7) board in
             p.piece_type <> Rook || p.piece_color <> Black)
            || snd !krook_moved );
        qrook_moved :=
          ( (let p = piece_at_pos (7, 0) board in
             p.piece_type <> Rook || p.piece_color <> White)
            || fst !qrook_moved,
            (let p = piece_at_pos (0, 0) board in
             p.piece_type <> Rook || p.piece_color <> Black)
            || snd !qrook_moved );
        if p.piece_type = King && snd end_pos - snd start_pos = 2 then (
          board_set
            (make_piece Blank None (fst start_pos, 7))
            (fst start_pos, 7)
            board;
          board_set
            (make_piece Rook turn (fst start_pos, snd end_pos - 1))
            (fst start_pos, snd end_pos - 1)
            board);
        if p.piece_type = King && snd end_pos - snd start_pos = -2 then (
          board_set
            (make_piece Blank None (fst start_pos, 0))
            (fst start_pos, 0)
            board;
          board_set
            (make_piece Rook turn (fst start_pos, snd end_pos + 1))
            (fst start_pos, snd end_pos + 1)
            board);
        (curr_game_state, true)))
    else (
      print_endline "Illegal move. Try again.";
      (curr_game_state, false))
  else (
    print_endline "Illegal move. Try again.";
    (curr_game_state, false))
