open Bap.Std
open Minos_types

(** Check template that provides utility functions *)

(** Return all the tids that are jumps. Typically used to check the
    values that the jmp are dependent on *)
val get_jmp_tids : path -> tid seq
