open Core_kernel.Std
open Bap.Std
open Spec
open Format

type t [@@deriving bin_io, compare, sexp]

type hypothesis = tid Pat.Map.t [@@deriving bin_io, compare, sexp]


val create : spec -> (defn * hypothesis) seq -> t

val annotate : t -> program term -> program term

val sat : t -> defn -> bool option

val pp_sat : defn -> formatter -> t -> unit

val pp_unsat : defn -> formatter -> t -> unit
