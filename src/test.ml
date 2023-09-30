open OUnit2
open Game

let _ = state_board
let tests = "chess test suite" >::: []
let _ = run_test_tt_main tests
