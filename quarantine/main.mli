open Core_kernel.Std
open Bap.Std
open Spec_types
open Taint


val run : program term -> int -> spec -> ident ->
  [`Cured of taints] *
  [`Uncured of taints] *
  [`Dead of taints]
