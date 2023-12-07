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

val make_piece : pieces -> color -> int * int -> piece
val get_piece_type : piece -> pieces
val get_piece_color : piece -> color
val get_piece_pos : piece -> int * int
val set_piece_pos : piece -> int * int -> piece
