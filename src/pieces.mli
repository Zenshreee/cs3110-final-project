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

(** [make_piece p c (x, y)] returns a piece of type [piece]. *)

val get_piece_type : piece -> pieces
(** [get_piece_type p] returns the type of piece [p]. *)

val get_piece_color : piece -> color
(** [get_piece_color p] returns the color of piece [p]. *)

val get_piece_pos : piece -> int * int
(** [get_piece_pos p] returns the position of piece [p]. *)

val set_piece_pos : piece -> int * int -> piece
(** [set_piece_pos p (x, y)] returns a piece of type [piece] with the same
    color and type as [p] but with position (x, y). *)
