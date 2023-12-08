open Pieces

type board = piece array array
(** [board] is the type of the chess board. *)

type last_move_type = {
  last_piece : piece;
  last_start_pos : int * int;
  last_end_pos : int * int;
}
(** [last_move_type] is the type of the last move made. It is a record with
    fields [last_piece], [last_start_pos], and [last_end_pos]. The [last_peice]
    is of type piece,representing the type of the piece that just moved the most
    recent iteration of the game loop. [last_start_pos] and [last_end_pos] are
    the starting and ending position of this recent move, respectively. *)

val testing : bool
(** [testing] determines whether or not the current turn is checked. if true,
    the turn is not being checking at each iteration of the game loop. *)

val position_of_string : string -> int * int
(** [position_of_string s] is the position of the string [s] in the form of a
    tuple of ints. *)

val within_bounds : int * int -> bool
(** [within_bounds (x,y)] is true if the position [(x,y)] is within the bounds
    of the board. *)

val piece_at_pos : int * int -> board -> piece
(** [piece_at_pos (x,y) b] is the piece at position [(x,y)] on board [b]. *)

val string_of_piece_type : pieces -> string
(** [string_of_piece_type p] is the string representation of the piece type [p]. *)

val pawn_checking : board -> int * int -> color -> bool
(** [pawn_checking] is true if the pawn at position [(x,y)] is checking the king
    of color [c]. *)

val knight_checking : board -> int * int -> color -> int * int -> bool
(** [knight_checking] is true if the knight at position [(x,y)] is checking the
    opponent's king which is at position [(x',y')]. *)

val check_line : board -> int * int -> color -> int * int -> bool
(** [check_line b (x,y) c (dx,dy)] is true if the king is under check along line
    from [(x,y)] to [(x+dx,y+dy)] is checking the king of color [c]. *)

val under_check : board -> color -> int * int -> bool
(** [under_check b c (x,y)] is true if the king of color [c] is under check at
    position [(x,y)]. *)

val check_path : board -> int * int -> int * int -> int * int -> color -> bool
(** [check_path b (dx,dy) (x,d) (x',y') c] is true if the path from [(x,y)] to
    [(x',y')] is clear along the direction [(dx, dy)] on the board [b], and the
    piece at the ending position [(x',y')] is not of the same color as [c]. *)

val is_en_passant_move :
  piece -> last_move_type -> int * int -> piece array array -> bool
(** [is_en_passant_move p l (x,y) b] is true if the move from [(x,y)] to
    [l.last_end_pos] is an en passant move. *)

val check_pawn : piece -> piece -> last_move_type -> piece array array -> bool
(** [check_pawn p l b] is true if the move from [p] to [l.last_end_pos] is a
    valid pawn move. *)

val check_knight : piece -> piece -> bool
(** [check_knight p l] is true if the move from [p] to [l.last_end_pos] is a
    valid knight move. *)

val check_king :
  piece -> piece -> color -> board -> bool -> bool -> bool -> bool
(** [check_king p l c b c1 c2 c3] is true if the move from [p] to
    [l.last_end_pos] is a valid king move. *)

val check_rook : board -> piece -> piece -> bool
(** [check_rook b p l] is true if the move from [p] to [l.last_end_pos] is a
    valid rook move. *)

val check_bishop : board -> piece -> piece -> bool
(** [check_bishop b p l] is true if the move from [p] to [l.last_end_pos] is a
    valid bishop move. *)

val check_queen : board -> piece -> piece -> bool
(** [check_queen b p l] is true if the move from [p] to [l.last_end_pos] is a
    valid queen move. *)

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
(** [valid_move b p l c lm b1 b2 b3 b4] is true if the move from [p] to
    [l.last_end_pos] is a valid move on board b. *)
