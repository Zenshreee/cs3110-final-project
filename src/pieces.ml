type color =
  | White
  | Black
  | None

type pieces =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Blank

type piece = {
  piece_type : pieces;
  piece_color : color;
  piece_pos : int * int;
}

let make_piece ptype pcolor ppos =
  { piece_type = ptype; piece_color = pcolor; piece_pos = ppos }
