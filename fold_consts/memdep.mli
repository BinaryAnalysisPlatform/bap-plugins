open Bap.Std

type t

(** [create ?memory arch sub] computes a memory model of a given
    subroutine [sub]. If static memory is provided, then it will
    be used to resolve memory accesses.*)
val create : ?memory:value memmap -> arch -> sub term -> t


(** [load model ~mem ~addr endian size] statically evaluates
    with respect to a computed memory [model].*)
val load : t -> mem:exp -> addr:exp -> endian -> size -> exp option
