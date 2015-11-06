open Bap.Std

type t = {sat : 'a. 'a term -> var -> bool}

val register : string -> t -> unit
val lookup : string -> t option
