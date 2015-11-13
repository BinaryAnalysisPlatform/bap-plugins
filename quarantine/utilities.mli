(** Useful utility functions

    Mostly on terms. Candidates for library promotion.*)
open Core_kernel.Std
open Bap.Std


(** [name_matches l id] returns true if label [l] is direct label
    with a given name. Example: [name_matches (Call.target c) "main"].*)
val label_matches : label -> string -> bool

(** [call_matches c id] is true if its target label matches  *)
val call_matches : call -> string -> bool

(** [intent_matches a in] is true if either argument's [a]
    intent is unknown, or if they have intersecting intents.*)
val intent_matches : arg term -> intent -> bool

(** [call_of_jmp j] is [Some call] if jmp is a call  *)
val call_of_jmp : jmp term -> call option

(** [callee c prog] returns a callee procedure of a given call [c] *)
val callee : call -> program term -> sub term option

(** [return c sub] finds a block to which call [c] will return *)
val return : call -> sub term -> blk term option

(** [require x] is [Some ()] if [x] and [None] otherwise.
    Useful, in option monad (c.f. haskell's [guard]):
    {[
      require (label_matches l "malloc") >>= fun () ->
      require (intent_matches a In) >>= fun () ->
    ]}

*)
val require : bool -> unit option
