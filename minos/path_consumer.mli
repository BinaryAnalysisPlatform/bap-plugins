open Bap.Std
open Trim
open Check

(** Path consumer dispatches path to a module that checks the path and
    returns an analysis result *)

(** Consume takes a path (after simplification) as a Sub.t. It also
    gets the global state. Consume dispatches the information to
    a checking module, and outputs the result in the path directory. *)
val consume : Sub.t -> Check.t -> Check.ctxt -> unit
