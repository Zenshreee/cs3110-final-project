open Pieces
open Array

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
