open Check
open Bap.Std

(** Check template that propvides utility functios *)

type arg = tid * Def.t * tid seq

(* 1 indexed *)
type args = {arg1 : arg option;
             arg2 : arg option;
             arg3 : arg option}

type path_attrs = {args : args;
                   jmp_deps : tid seq;
                   num_blks : int}

val get_jmp_tids : Sub.t -> tid seq

val infer_arg : ctxt -> Blk.t -> string -> arg option

val infer_args : ctxt -> Blk.t -> args

val get_jmp_deps : Sub.t -> tid seq

val populate_deps : Sub.t -> tid * Def.t * 'a -> arg

val infer_args_with_deps : ctxt -> Blk.t -> args
