open Bap.Std

type t = {sat : 'a. 'a term -> exp -> bool}

val register : string -> t -> unit
val lookup : string -> t option
val test : string -> 'a term -> exp -> bool
