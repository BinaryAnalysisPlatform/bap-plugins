open Core_kernel.Std
open Bap.Std
open Spec_types

type t


val create : spec -> t

val seed_sub : t -> sub term -> sub term

val seed : t -> program term -> program term
