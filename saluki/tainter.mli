open Bap.Std
open Spec

type t

val seed : spec -> program term -> program term

val reap : program term -> t

val ptrs_of_var : t -> tid -> var -> Taint.set

val regs_of_var : t -> tid -> var -> Taint.set

val ptr_seed_of_var : t -> tid -> var -> Taint.t option

val reg_seed_of_var : t -> tid -> var -> Taint.t option
