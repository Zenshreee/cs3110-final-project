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

let make_piece (ptype : pieces) (pcolor : color) (ppos : int * int) =
  { piece_type = ptype; piece_color = pcolor; piece_pos = ppos }

let get_piece_type (p : piece) : pieces = p.piece_type
let get_piece_color (p : piece) : color = p.piece_color
let get_piece_pos (p : piece) : int * int = p.piece_pos

let set_piece_pos (p : piece) (pos : int * int) : piece =
  let p_type = p.piece_type in
  let p_col = p.piece_color in
  make_piece p_type p_col pos
