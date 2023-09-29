type t
(** The type of BRB accounts. *)

val make : t
(** An account with a zero balance. *)

val balance : t -> int
(** [balance acct] is the balance of [acct]. *)

val deposit : int -> t -> t
(** [deposit amt acct] is [acct] with [amt] deposited into it. *)

val pay : int -> t -> t
(** [pay amt acct] is [acct] with [amt] payed out from it. *)
