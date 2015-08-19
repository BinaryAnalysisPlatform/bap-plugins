open Bap.Std

type t

val create : ?memory:value memmap -> (var -> bool) -> sub term -> t

val lookup : t -> var -> exp -> exp option
