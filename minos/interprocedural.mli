open Bap.Std
open Filter

val inline_fixpoint : ?warn:bool -> project -> Sub.t -> Filter.t ->
  Sub.t * (string * int) list

val inline_n : ?warn:bool -> project ->
sub term -> Filter.t -> int -> Sub.t * (string * int) list
