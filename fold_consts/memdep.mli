open Bap.Std

type t

val create : (var -> bool) -> sub term -> t

val lookup : t -> var -> exp -> exp option
