(************************************************************

   Test Plan

   Automatic: 
   Manual: Due to the difficulty with checking the board state after castling,en passant, and pawn promotion, we decided to manually test. Starting with the initial game state we played the game and attempted to castle, perform en passant, and pawn promotion under various scenarios. A few of the move configurations are in the data folder as text files for reference.
    - Castling:
        - Attempted to castle after king has moved
        - Attempted castling after kingside rook moved
        - Attempted castling after queenside rook moved
        - Attempted castling while king under check
        - Attempted castling when one of the squares that the king passes through is under check
        - Proper castling
        - Did the above with both black and white
    - En Passant
      - Correct en passant
      - Attempt en passant too late
      - Attempt en passant with pawn absent
      - Tested above for both white and black
    - Pawn Promotion
      - Tested promotion to each type of allowed piece
      - Tested promotion for piece not allowed
      - Tested movement of promoted piece
      - Tested pawn not allowed to promote (move to last square)
      - Did above with both black and white

   Black Box Testing: We tested check_pawn, check_knight, check_bishop, check_rook, check_queen, check_king to check valid and invalid moves for each piece. This extensive testing covered moving a piece in all possible directions, and testing the validity of the move based on other pieces on the board.
   Glass Box Testing: We tested under_check, castling and other complex moves using glass box testing and used the control logic of the methods written to ensure good coverage. For example, checking that last_move is correct after an illegal move is attempted.
   Randomized Testing: We did not think it would be reasonable to attempt random moves as the vast majority of moves would be illegal. Instead, we decided to randomly move manually and assess how the game state changes.

   Why this demonstrates correctness:

   Our program is intended to play chess following standard chess rules including castling, en passant, and pawn promotion. Each move type has a check function that checks if a given move is valid. We extensively tested this function for pawns, knights, bishops, kings, queens, and rooks. This way we can ensure that legal moves are played properly. We also check incorrect moves by determining whether our check moves function returns false. After manually testing castling, en passant, and pawn promotion from many different starting configurations we are confident.

   Our program's game flow executes exactly as intended as well as demostrated by the countless games we played. Ensuring the correctness of our tests thus demonstrates the correctness of our chess program, since users are only permitted to make valid chess moves that are specified by the game rules.

 ************************************************************)

open OUnit2
open Chess
open Board
open Moves
open Pieces

let init_board = board
let k_test color pos = make_piece Knight color pos
let b_test color = make_piece Bishop color (4, 4)
let r_test color = make_piece Rook color (4, 4)
let king_test color = make_piece King color (4, 4)

let check_pawn_tests =
  [
    ( "pawn forward 1" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal true
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (5, 0) board)
           last_move board) );
    ( "pawn forward 2" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal true
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (4, 0) board)
           last_move board) );
    ( "pawn forward 3" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (3, 0) board)
           last_move board) );
    ( "pawn forward 4" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (2, 0) board)
           last_move board) );
    ( "pawn forward 5" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (6, 1) board)
           last_move board) );
    ( "pawn forward 6" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (6, 2) board)
           last_move board) );
    ( "pawn forward 7" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (6, 3) board)
           last_move board) );
    ( "pawn forward 8" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (0, 0); last_end_pos = (0, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (6, 0) board)
           (piece_at_pos (6, 4) board)
           last_move board) );
    ( "pawn forward 9" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (6, 0); last_end_pos = (5, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (5, 0) board)
           (piece_at_pos (4, 0) board)
           last_move board) );
    ( "pawn forward 10" >:: fun _ ->
      let p = make_piece Blank None (7, 7) in
      let last_move =
        { last_piece = p; last_start_pos = (6, 0); last_end_pos = (5, 0) }
      in
      assert_equal false
        (check_pawn
           (piece_at_pos (5, 0) board)
           (piece_at_pos (3, 0) board)
           last_move board) );
  ]

let check_knight_tests =
  [
    ( "white knight down 1 right 2" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (5, 6) board)) );
    ( "white knight down 1 left 2" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (5, 2) board)) );
    ( "white knight up 1 right 2" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (3, 6) board)) );
    ( "white knight up 1 left 2" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (3, 2) board)) );
    ( "white knight down 2 right 1" >:: fun _ ->
      assert_equal false
        (check_knight (k_test White (4, 4)) (piece_at_pos (6, 5) board)) );
    ( "white knight down 2 left 1" >:: fun _ ->
      assert_equal false
        (check_knight (k_test White (4, 4)) (piece_at_pos (6, 3) board)) );
    ( "white knight up 2 right 1" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (2, 5) board)) );
    ( "white knight up 2 left 1" >:: fun _ ->
      assert_equal true
        (check_knight (k_test White (4, 4)) (piece_at_pos (2, 3) board)) );
    ( "black knight down 1 right 2 " >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (4, 6) board)) );
    ( "black knight down 1 left 2 " >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (4, 2) board)) );
    ( "black knight up 1 right 2 " >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (2, 6) board)) );
    ( "black knight up 1 left 2 " >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (2, 2) board)) );
    ( "black knight down 2 right 1 " >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (5, 5) board)) );
    ( "black knight down 2 left 1" >:: fun _ ->
      assert_equal true
        (check_knight (k_test Black (3, 4)) (piece_at_pos (5, 3) board)) );
    ( "black knight up 2 right 1" >:: fun _ ->
      assert_equal false
        (check_knight (k_test Black (3, 4)) (piece_at_pos (1, 5) board)) );
    ( "black knight up 2 left 1" >:: fun _ ->
      assert_equal false
        (check_knight (k_test Black (3, 4)) (piece_at_pos (1, 3) board)) );
  ]

let check_bishop_tests =
  [
    ( "white bishop up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (3, 3) board)) );
    ( "white bishop up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (3, 5) board)) );
    ( "white bishop down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (5, 3) board)) );
    ( "white bishop down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (5, 5) board)) );
    ( "white bishop up 2 left 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (2, 2) board)) );
    ( "white bishop up 2 right 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (2, 6) board)) );
    ( "white bishop down 2 left 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (6, 2) board)) );
    ( "white bishop down 2 right 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (6, 6) board)) );
    ( "white bishop up 1 left 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (3, 2) board)) );
    ( "white bishop up 1 right 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (3, 6) board)) );
    ( "white bishop down 1 left 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (5, 2) board)) );
    ( "white bishop down 1 right 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (5, 6) board)) );
    ( "white bishop up 2 left 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (2, 3) board)) );
    ( "white bishop up 2 right 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (2, 5) board)) );
    ( "white bishop down 2 left 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (6, 3) board)) );
    ( "white bishop down 2 right 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (6, 5) board)) );
    ( "white bishop top right" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (2, 6) board)) );
    ( "white bishop top left" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test White) (piece_at_pos (2, 2) board)) );
    ( "white bishop bottom right" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (7, 7) board)) );
    ( "white bishop bottom left" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test White) (piece_at_pos (7, 1) board)) );
    ( "black bishop up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (3, 3) board)) );
    ( "black bishop up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (3, 5) board)) );
    ( "black bishop down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (5, 3) board)) );
    ( "black bishop down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (5, 5) board)) );
    ( "black bishop up 2 left 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (2, 2) board)) );
    ( "black bishop up 2 right 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (2, 6) board)) );
    ( "black bishop down 2 left 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (6, 2) board)) );
    ( "black bishop down 2 right 2" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (6, 6) board)) );
    ( "black bishop up 1 left 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (3, 2) board)) );
    ( "black bishop up 1 right 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (3, 6) board)) );
    ( "black bishop down 1 left 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (5, 2) board)) );
    ( "black bishop down 1 right 2" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (5, 6) board)) );
    ( "black bishop up 2 left 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (2, 3) board)) );
    ( "black bishop up 2 right 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (2, 5) board)) );
    ( "black bishop down 2 left 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (6, 3) board)) );
    ( "black bishop down 2 right 1" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (6, 5) board)) );
    ( "black bishop top right" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (2, 6) board)) );
    ( "black bishop top left" >:: fun _ ->
      assert_equal true
        (check_bishop board (b_test Black) (piece_at_pos (2, 2) board)) );
    ( "black bishop bottom right" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (7, 7) board)) );
    ( "black bishop bottom left" >:: fun _ ->
      assert_equal false
        (check_bishop board (b_test Black) (piece_at_pos (7, 1) board)) );
  ]

let check_rook_tests =
  [
    ( "white rook down 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (5, 4) board)) );
    ( "white rook down 2" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test White) (piece_at_pos (6, 4) board)) );
    ( "white rook all the way down" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test White) (piece_at_pos (7, 4) board)) );
    ( "white rook up 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (3, 4) board)) );
    ( "white rook up 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (2, 4) board)) );
    ( "white rook up 3" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (1, 4) board)) );
    ( "white rook all the way up" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test White) (piece_at_pos (0, 4) board)) );
    ( "white rook right 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 5) board)) );
    ( "white rook right 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 6) board)) );
    ( "white rook all the way right" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 7) board)) );
    ( "white rook left 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 3) board)) );
    ( "white rook left 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 2) board)) );
    ( "white rook left 3" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 1) board)) );
    ( "white rook all the way left" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test White) (piece_at_pos (4, 0) board)) );
    ( "black rook down 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (5, 4) board)) );
    ( "black rook down 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (6, 4) board)) );
    ( "black rook all the way down" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test Black) (piece_at_pos (7, 4) board)) );
    ( "black rook up 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (3, 4) board)) );
    ( "black rook up 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (2, 4) board)) );
    ( "black rook up 3" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test Black) (piece_at_pos (1, 4) board)) );
    ( "black rook all the way up" >:: fun _ ->
      assert_equal false
        (check_rook board (r_test Black) (piece_at_pos (0, 4) board)) );
    ( "Black rook right 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 5) board)) );
    ( "Black rook right 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 6) board)) );
    ( "Black rook all the way right" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 7) board)) );
    ( "Black rook left 1" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 3) board)) );
    ( "Black rook left 2" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 2) board)) );
    ( "black rook left 3" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 1) board)) );
    ( "black rook all the way left" >:: fun _ ->
      assert_equal true
        (check_rook board (r_test Black) (piece_at_pos (4, 0) board)) );
  ]

let check_queen_tests =
  [
    ( "white queen up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (3, 3) board)) );
    ( "white queen up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (3, 5) board)) );
    ( "white queen down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (5, 3) board)) );
    ( "white queen down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (5, 5) board)) );
    ( "white queen up 2 left 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (2, 2) board)) );
    ( "white queen up 2 right 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (2, 6) board)) );
    ( "white queen down 2 left 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (6, 2) board)) );
    ( "white queen down 2 right 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (6, 6) board)) );
    ( "white queen up 1 left 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (3, 2) board)) );
    ( "white queen up 1 right 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (3, 6) board)) );
    ( "white queen down 1 left 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (5, 2) board)) );
    ( "white queen down 1 right 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (5, 6) board)) );
    ( "white queen up 2 left 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (2, 3) board)) );
    ( "white queen up 2 right 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (2, 5) board)) );
    ( "white queen down 2 left 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (6, 3) board)) );
    ( "white queen down 2 right 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (6, 5) board)) );
    ( "white queen top right" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (2, 6) board)) );
    ( "white queen top left" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test White) (piece_at_pos (2, 2) board)) );
    ( "white queen bottom right" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (7, 7) board)) );
    ( "white queen bottom left" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test White) (piece_at_pos (7, 1) board)) );
    ( "black queen up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (3, 3) board)) );
    ( "black queen up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (3, 5) board)) );
    ( "black queen down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (5, 3) board)) );
    ( "black queen down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (5, 5) board)) );
    ( "black queen up 2 left 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (2, 2) board)) );
    ( "black queen up 2 right 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (2, 6) board)) );
    ( "black queen down 2 left 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (6, 2) board)) );
    ( "black queen down 2 right 2" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (6, 6) board)) );
    ( "black queen up 1 left 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (3, 2) board)) );
    ( "black queen up 1 right 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (3, 6) board)) );
    ( "black queen down 1 left 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (5, 2) board)) );
    ( "black queen down 1 right 2" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (5, 6) board)) );
    ( "black queen up 2 left 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (2, 3) board)) );
    ( "black queen up 2 right 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (2, 5) board)) );
    ( "black queen down 2 left 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (6, 3) board)) );
    ( "black queen down 2 right 1" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (6, 5) board)) );
    ( "black queen top right" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (2, 6) board)) );
    ( "black queen top left" >:: fun _ ->
      assert_equal true
        (check_queen board (b_test Black) (piece_at_pos (2, 2) board)) );
    ( "black queen bottom right" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (7, 7) board)) );
    ( "black queen bottom left" >:: fun _ ->
      assert_equal false
        (check_queen board (b_test Black) (piece_at_pos (7, 1) board)) );
    ( "white queen down 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (5, 4) board)) );
    ( "white queen down 2" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test White) (piece_at_pos (6, 4) board)) );
    ( "white queen all the way down" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test White) (piece_at_pos (7, 4) board)) );
    ( "white queen up 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (3, 4) board)) );
    ( "white queen up 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (2, 4) board)) );
    ( "white queen up 3" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (1, 4) board)) );
    ( "white queen all the way up" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test White) (piece_at_pos (0, 4) board)) );
    ( "white queen right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 5) board)) );
    ( "white queen right 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 6) board)) );
    ( "white queen all the way right" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 7) board)) );
    ( "white queen left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 3) board)) );
    ( "white queen left 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 2) board)) );
    ( "white queen left 3" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 1) board)) );
    ( "white queen all the way left" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test White) (piece_at_pos (4, 0) board)) );
    ( "black queen down 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (5, 4) board)) );
    ( "black queen down 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (6, 4) board)) );
    ( "black queen all the way down" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test Black) (piece_at_pos (7, 4) board)) );
    ( "black queen up 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (3, 4) board)) );
    ( "black queen up 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (2, 4) board)) );
    ( "black queen up 3" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test Black) (piece_at_pos (1, 4) board)) );
    ( "black queen all the way up" >:: fun _ ->
      assert_equal false
        (check_queen board (r_test Black) (piece_at_pos (0, 4) board)) );
    ( "Black queen right 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 5) board)) );
    ( "Black queen right 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 6) board)) );
    ( "Black queen all the way right" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 7) board)) );
    ( "Black queen left 1" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 3) board)) );
    ( "Black queen left 2" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 2) board)) );
    ( "black queen left 3" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 1) board)) );
    ( "black queen all the way left" >:: fun _ ->
      assert_equal true
        (check_queen board (r_test Black) (piece_at_pos (4, 0) board)) );
  ]

let check_king_tests =
  [
    ( "white king down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (5, 3) board)
           White board false false false) );
    ( "white king down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (5, 4) board)
           White board false false false) );
    ( "white king up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (3, 3) board)
           White board false false false) );
    ( "white king up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (3, 5) board)
           White board false false false) );
    ( "white king up 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (3, 4) board)
           White board false false false) );
    ( "white king up 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test White)
           (piece_at_pos (2, 4) board)
           White board false false false) );
    ( "white king down 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (5, 4) board)
           White board false false false) );
    ( "white king down 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test White)
           (piece_at_pos (6, 4) board)
           White board false false false) );
    ( "white king right 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (4, 5) board)
           White board false false false) );
    ( "white king right 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test White)
           (piece_at_pos (4, 6) board)
           White board false false false) );
    ( "white king left 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test White)
           (piece_at_pos (4, 3) board)
           White board false false false) );
    ( "white king left 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test White)
           (piece_at_pos (4, 2) board)
           White board false false false) );
    ( "black king down 1 left 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (5, 3) board)
           White board false false false) );
    ( "black king down 1 right 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (5, 5) board)
           White board false false false) );
    ( "black king up 1 left 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (3, 3) board)
           White board false false false) );
    ( "black king up 1 right 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (3, 5) board)
           White board false false false) );
    ( "black king up 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (3, 4) board)
           White board false false false) );
    ( "black king up 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test Black)
           (piece_at_pos (2, 4) board)
           White board false false false) );
    ( "black king down 1" >:: fun _ ->
      assert_equal true
        (check_king (king_test Black)
           (piece_at_pos (5, 4) board)
           White board false false false) );
    ( "black king down 2" >:: fun _ ->
      assert_equal false
        (check_king (king_test Black)
           (piece_at_pos (6, 4) board)
           White board false false false) );
  ]

let check_board1 =
  [|
    [|
      make_piece Rook Black (0, 0);
      make_piece Knight Black (0, 1);
      make_piece Bishop Black (0, 2);
      make_piece Queen Black (0, 3);
      make_piece King Black (0, 4);
      make_piece Bishop Black (0, 5);
      make_piece Knight Black (0, 6);
      make_piece Rook Black (0, 7);
    |];
    [|
      make_piece Pawn Black (1, 0);
      make_piece Pawn Black (1, 1);
      make_piece Pawn Black (1, 2);
      make_piece Pawn Black (1, 3);
      make_piece Pawn Black (1, 4);
      make_piece Pawn Black (1, 5);
      make_piece Pawn Black (1, 6);
      make_piece Pawn Black (1, 7);
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
      make_piece Pawn White (6, 0);
      make_piece Pawn White (6, 1);
      make_piece Pawn White (6, 2);
      make_piece Pawn White (6, 3);
      make_piece Pawn White (6, 4);
      make_piece Pawn White (6, 5);
      make_piece Pawn White (6, 6);
      make_piece Pawn White (6, 7);
    |];
    [|
      make_piece Rook White (7, 0);
      make_piece Knight White (7, 1);
      make_piece Bishop White (7, 2);
      make_piece Queen White (7, 3);
      make_piece King White (7, 4);
      make_piece Bishop White (7, 5);
      make_piece Knight White (7, 6);
      make_piece Rook White (7, 7);
    |];
  |]

let _ = make_move "b1 c3" check_board1 White
let _ = make_move "h7 h6" check_board1 Black
let _ = make_move "c3 b5" check_board1 White
let _ = make_move "h6 h5" check_board1 Black
let _ = make_move "b5 c7" check_board1 White

let check_board2 =
  [|
    [|
      make_piece Rook Black (0, 0);
      make_piece Knight Black (0, 1);
      make_piece Bishop Black (0, 2);
      make_piece Queen Black (0, 3);
      make_piece King Black (0, 4);
      make_piece Bishop Black (0, 5);
      make_piece Knight Black (0, 6);
      make_piece Rook Black (0, 7);
    |];
    [|
      make_piece Pawn Black (1, 0);
      make_piece Pawn Black (1, 1);
      make_piece Pawn Black (1, 2);
      make_piece Pawn Black (1, 3);
      make_piece Pawn Black (1, 4);
      make_piece Pawn Black (1, 5);
      make_piece Pawn Black (1, 6);
      make_piece Pawn Black (1, 7);
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
      make_piece Pawn White (6, 0);
      make_piece Pawn White (6, 1);
      make_piece Pawn White (6, 2);
      make_piece Pawn White (6, 3);
      make_piece Pawn White (6, 4);
      make_piece Pawn White (6, 5);
      make_piece Pawn White (6, 6);
      make_piece Pawn White (6, 7);
    |];
    [|
      make_piece Rook White (7, 0);
      make_piece Knight White (7, 1);
      make_piece Bishop White (7, 2);
      make_piece Queen White (7, 3);
      make_piece King White (7, 4);
      make_piece Bishop White (7, 5);
      make_piece Knight White (7, 6);
      make_piece Rook White (7, 7);
    |];
  |]

let _ = make_move "a2 a4" check_board2 White
let _ = make_move "h7 h6" check_board2 Black
let _ = make_move "a1 a3" check_board2 White
let _ = make_move "h6 h5" check_board2 Black
let _ = make_move "a3 b3" check_board2 White
let _ = make_move "h5 h4" check_board2 Black
let _ = make_move "b3 b7" check_board2 White
let _ = make_move "h4 h3" check_board2 Black
let _ = make_move "b7 b8" check_board2 White
let _ = make_move "h3 g2" check_board2 Black
let _ = make_move "b8 c8" check_board2 White
let _ = make_move "g7 g6" check_board2 Black
let _ = make_move "c8 d8" check_board2 White

let check_board3 =
  [|
    [|
      make_piece Rook Black (0, 0);
      make_piece Knight Black (0, 1);
      make_piece Bishop Black (0, 2);
      make_piece Queen Black (0, 3);
      make_piece King Black (0, 4);
      make_piece Bishop Black (0, 5);
      make_piece Knight Black (0, 6);
      make_piece Rook Black (0, 7);
    |];
    [|
      make_piece Pawn Black (1, 0);
      make_piece Pawn Black (1, 1);
      make_piece Pawn Black (1, 2);
      make_piece Pawn Black (1, 3);
      make_piece Pawn Black (1, 4);
      make_piece Pawn Black (1, 5);
      make_piece Pawn Black (1, 6);
      make_piece Pawn Black (1, 7);
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
      make_piece Pawn White (6, 0);
      make_piece Pawn White (6, 1);
      make_piece Pawn White (6, 2);
      make_piece Pawn White (6, 3);
      make_piece Pawn White (6, 4);
      make_piece Pawn White (6, 5);
      make_piece Pawn White (6, 6);
      make_piece Pawn White (6, 7);
    |];
    [|
      make_piece Rook White (7, 0);
      make_piece Knight White (7, 1);
      make_piece Bishop White (7, 2);
      make_piece Queen White (7, 3);
      make_piece King White (7, 4);
      make_piece Bishop White (7, 5);
      make_piece Knight White (7, 6);
      make_piece Rook White (7, 7);
    |];
  |]

let _ = make_move "a2 a4" check_board3 White
let _ = make_move "h7 h6" check_board3 Black
let _ = make_move "a4 a5" check_board3 White
let _ = make_move "h6 h5" check_board3 Black
let _ = make_move "c2 c3" check_board3 White
let _ = make_move "d7 d6" check_board3 Black
let _ = make_move "d1 a4" check_board3 White
let _ = make_move "h5 h4" check_board3 Black
let _ = make_move "b7 b5" check_board3 Black
let _ = make_move "a5 b6" check_board3 White

let check_tests =
  [
    ( "check black" >:: fun _ ->
      assert_equal true (under_check check_board1 Black (0, 4)) );
    ( "check black 2" >:: fun _ ->
      assert_equal true (under_check check_board2 Black (0, 4)) );
    ( "check black 3" >:: fun _ ->
      assert_equal true (under_check check_board3 Black (0, 4)) );
  ]

let tests =
  "chess test suite"
  >::: List.flatten
         [
           check_pawn_tests;
           check_knight_tests;
           check_bishop_tests;
           check_rook_tests;
           check_queen_tests;
           check_king_tests;
           check_tests;
         ]

let _ = run_test_tt_main tests
