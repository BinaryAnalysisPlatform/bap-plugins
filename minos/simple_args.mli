open Bap.Std

(** arg tid and def *)
type arg = tid * Def.t

(** A selection of args we can pattern match against in checks. 1 indexed *)
type args = {arg1 : arg option;
             arg2 : arg option;
             arg3 : arg option;
             arg4 : arg option}


(** Given a blk, return an arg option corresponding to the string,
    where the string is "arg1", "arg2", ... *)
val infer_arg : project -> Blk.t -> string -> arg option

(** Return all the args found in this blk *)
val infer_args : project -> Blk.t -> args
