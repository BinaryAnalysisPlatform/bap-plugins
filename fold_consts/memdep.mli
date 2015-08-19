open Bap.Std

type t

val create : ?memory:value memmap -> arch -> sub term -> t


(** [lookup model mem]   *)
val lookup : t -> var -> exp -> exp option
(* TODO: looks like that we may need to provide more information, or
   provide two function: as to read from memory we need to know type.
*)
