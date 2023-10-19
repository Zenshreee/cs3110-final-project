open OUnit2
(* open Game *)

let tests = "chess test suite" >::: []
let _ = run_test_tt_main tests
