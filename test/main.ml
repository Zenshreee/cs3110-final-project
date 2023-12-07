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
         ]

let _ = run_test_tt_main tests
