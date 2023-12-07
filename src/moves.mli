open Pieces

type board = piece array array

type last_move_type = {
  last_piece : piece;
  last_start_pos : int * int;
  last_end_pos : int * int;
}

val testing : bool
val position_of_string : string -> int * int
val within_bounds : int * int -> bool
val piece_at_pos : int * int -> board -> piece
val string_of_piece_type : pieces -> string
val pawn_checking : board -> int * int -> color -> bool
val knight_checking : board -> int * int -> color -> int * int -> bool
val check_line : board -> int * int -> color -> int * int -> bool
val under_check : board -> color -> int * int -> bool
val check_path : board -> int * int -> int * int -> int * int -> color -> bool

val is_en_passant_move :
  piece -> last_move_type -> int * int -> piece array array -> bool

val check_pawn : piece -> piece -> last_move_type -> piece array array -> bool
val check_knight : piece -> piece -> bool

val check_king :
  piece -> piece -> color -> board -> bool -> bool -> bool -> bool

val check_rook : board -> piece -> piece -> bool
val check_bishop : board -> piece -> piece -> bool
val check_queen : board -> piece -> piece -> bool

val valid_move :
  board ->
  piece ->
  int * int ->
  color ->
  last_move_type ->
  bool ->
  bool ->
  bool ->
  bool

val piece_at_pos : int * int -> piece array array -> piece
val position_of_string : string -> int * int
