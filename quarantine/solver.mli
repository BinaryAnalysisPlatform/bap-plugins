open Core_kernel.Std
open Bap.Std
open Spec_types

type t
type solution


val create : spec -> t

val seed_sub : t -> program term -> sub term -> program term

val seed : t -> program term -> program term

val solve : t -> program term -> solution

val pp_solution : Format.formatter -> solution -> unit
