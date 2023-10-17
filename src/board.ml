open Pieces

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

let print_board =
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
(* print_string "\n________________________________\n"; *)
