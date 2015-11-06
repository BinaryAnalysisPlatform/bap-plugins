open Core_kernel.Std
open Bap.Std
open Spec_types

type t
type solution
type category = [
  | `satisfied
  | `unsatisfied
  | `unrecognized
]

val create : spec -> t

val seed_sub : t -> program term -> sub term -> sub term

val seed : t -> program term -> program term

val solve : t -> program term -> solution


val pp_solution : category -> Format.formatter -> solution -> unit
