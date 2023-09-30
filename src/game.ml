open Board

exception GameEnd
(** [GameEnd] is an exception.*)

(** black_winner is var of type [string]. It represents graphic displayed when
    black wins.*)

let init_board = board
let state_board = print_board
