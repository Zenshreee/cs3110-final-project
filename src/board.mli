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
(** [white_pieces] is the type that represents the white player's pieces in
    ASCII art. *)

type black_pieces = {
  king : string;
  queen : string;
  rook : string;
  knight : string;
  bishop : string;
  pawn : string;
}
(** [black_pieces] is the type that represents the black player's pieces in
    ASCII art. *)

val board : piece array array
(** [board] represents the chess board. *)

val king_loc : ((int * int) * (int * int)) ref
(** [king_loc] is the location of the king. *)

val king_moved : (bool * bool) ref
(** [king_moved] is a reference to a tuple of booleans that represent whether
    the king has moved. The first boolean is true if the king has moved and the
    second boolean is true if the king has castled. *)

val krook_moved : (bool * bool) ref
(** [krook_moved] is a reference to a tuple of booleans that represent whether
    the king's rook has moved. The first boolean is true if the king's rook has
    moved and the second boolean is true if the king's rook has castled. *)

val qrook_moved : (bool * bool) ref
(** [qrook_moved] is a reference to a tuple of booleans that represent whether
    the queen's rook has moved. The first boolean is true if the queen's rook
    has moved and the second boolean is true if the queen's rook has castled. *)

val last_move : last_move_type ref
(** [last_move] is the last move made. *)

val print_board : piece array array -> unit
(** [print_board b] prints the board [b]. *)

val print_tuple : int * int -> unit
(** [print_tuple (x,y)] prints the tuple [(x,y)]. *)

val board_set : piece -> int * int -> piece array array -> unit
(** [board_set p (x,y) b] sets the piece [p] at the location [(x,y)] on the
    board [b]. *)

val is_en_passant_move :
  piece -> last_move_type -> int * int -> piece array array -> bool
(** [is_en_passant_move p lm (x,y) b] is true if the move [(x,y)] is an en
    passant move. *)

val is_pawn_promotion : piece -> int * int -> piece array array -> bool
(** [is_pawn_promotion p (x,y) b] is true if the piece [p] is a pawn and is at
    the end of the board [b]. *)

val update_board_and_last_move :
  piece -> int * int -> int * int -> piece array array -> unit
(** [update_board_and_last_move p (x,y) (x',y') b] updates the board [b] and the
    last move to [(x,y) -> (x',y')] with the piece [p]. *)

val ask_match_choice : unit -> pieces
(** [ask_match_choice ()] asks the user what type of match they want to play and
    returns the type of match. *)

val make_move : string -> piece array array -> color -> bool
(** [make_move s b c] makes the move [s] on the board [b] for the player with
    color [c]. *)
