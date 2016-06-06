open Bap.Std
open Spec

type t

val create : spec -> Tainter.t -> t

val start_conclusions : t -> t

(** [step state term matcher] given a function [matcher] that for each
    pattern returns a set of matching variables, performs one step of
    search of matching terms.*)
val step : t -> 'a term -> (pat -> Match.t) -> t

(** [solutions state spec] returns current solution  *)
val solution : t -> spec -> Solution.t


val pp : Format.formatter -> t -> unit
