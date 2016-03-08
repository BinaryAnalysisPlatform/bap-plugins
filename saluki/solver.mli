open Core_kernel.Std
open Bap.Std
open Spec


(** [run ?on_step init prog] searches for solutions using [State.step] in
    the program term. Function [on_state] will be called each time a
    step is finished (usefull for fancy progress bars).

    The worst-case complexity is [O((r+1)^n)], where [r] is total
    number of different rules in specification. The worst case
    complexity is achieved when every term matches every rule, of
    course, so in general the complexity is closer to linear, since
    the problem state tends to remain of the same size.

    Note: equal rules in different definitions are still considered
    distinct. *)
val run : ?on_step:(State.t -> unit) -> State.t -> program term -> State.t
