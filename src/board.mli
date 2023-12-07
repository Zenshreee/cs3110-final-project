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

val white_pieces : white_pieces

type black_pieces = {
  king : string;
  queen : string;
  rook : string;
  knight : string;
  bishop : string;
  pawn : string;
}

val black_pieces : black_pieces

type board = piece array array

val board : piece array array
val king_loc : ((int * int) * (int * int)) ref
val king_moved : (bool * bool) ref
val krook_moved : (bool * bool) ref
val qrook_moved : (bool * bool) ref
val last_move : last_move_type ref
val print_board : piece array array -> unit
val print_tuple : int * int -> unit
val board_set : piece -> int * int -> piece array array -> unit

val is_en_passant_move :
  piece -> last_move_type -> int * int -> piece array array -> bool

val is_pawn_promotion : piece -> int * 'a -> 'b -> bool

val update_board_and_last_move :
  piece -> int * int -> int * int -> piece array array -> unit

val ask_match_choice : unit -> pieces
val make_move : string -> piece array array -> color -> bool
