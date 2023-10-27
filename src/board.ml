open Pieces
open Moves

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

let ranks = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]

(* rows *)
let files = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]

(* columns *)
let board =
  [|
    [|
      make_piece Rook Black (7, 0);
      make_piece Knight Black (7, 1);
      make_piece Bishop Black (7, 2);
      make_piece Queen Black (7, 3);
      make_piece King Black (7, 4);
      make_piece Bishop Black (7, 5);
      make_piece Knight Black (7, 6);
      make_piece Rook Black (7, 7);
    |];
    [|
      make_piece Pawn Black (6, 0);
      make_piece Pawn Black (6, 1);
      make_piece Pawn Black (6, 2);
      make_piece Pawn Black (6, 3);
      make_piece Pawn Black (6, 4);
      make_piece Pawn Black (6, 5);
      make_piece Pawn Black (6, 6);
      make_piece Pawn Black (6, 7);
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
      make_piece Pawn White (1, 0);
      make_piece Pawn White (1, 1);
      make_piece Pawn White (1, 2);
      make_piece Pawn White (1, 3);
      make_piece Pawn White (1, 4);
      make_piece Pawn White (1, 5);
      make_piece Pawn White (1, 6);
      make_piece Pawn White (1, 7);
    |];
    [|
      make_piece Rook White (0, 0);
      make_piece Knight White (0, 1);
      make_piece Bishop White (0, 2);
      make_piece Queen White (0, 3);
      make_piece King White (0, 4);
      make_piece Bishop White (0, 5);
      make_piece Knight White (0, 6);
      make_piece Rook White (0, 7);
    |];
  |]

let print_board board =
  for i = 0 to 7 do
    print_string "\n________________________________\n";
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
          | _ -> print_string " ")
      | { piece_type = p; piece_color = White; piece_pos = _ } -> (
          match p with
          | Pawn -> print_string white_pieces.pawn
          | Rook -> print_string white_pieces.rook
          | Knight -> print_string white_pieces.knight
          | Bishop -> print_string white_pieces.bishop
          | Queen -> print_string white_pieces.queen
          | King -> print_string white_pieces.king
          | _ -> print_string " ")
      | _ -> print_string " "
    done
  done

(*Sets a piece on board curr at position pos*)
let board_set piece pos curr =
  let z, w = pos in
  let row = curr.(z) in
  let p = set_piece_pos piece pos in
  let _ = row.(w) <- p in
  let _ = curr.(z) <- row in
  curr

(* print_string "\n________________________________\n"; *)
(* Precondition: Input must be in chess notation. For example "E4 E5"*)
let make_move (m : string) (curr : piece array array) : piece array array =
  let start_pos = position_of_string (String.sub m 0 2) in
  let end_pos = position_of_string (String.sub m 3 2) in

  (*Placeholder code for demo purposes*)
  let p = piece_at_pos start_pos curr in

  (*Set the place where the piece started to blank*)
  let new_board = board_set (make_piece Blank None start_pos) start_pos curr in
  (*Set the piece to the end position*)
  let final_board = board_set p end_pos new_board in
  final_board
