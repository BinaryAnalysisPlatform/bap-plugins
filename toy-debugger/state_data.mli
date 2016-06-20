open Core_kernel.Std
open Bap.Std
open Flag

(** if no directory is sepcified, it prints to standard out *)
val to_csv : ?dirname:string -> invoker:hook ->
  [< `Memory | `Path_counts | `Regs | `Trace | `Myself | `Checkpoints]
  -> int -> string list list -> unit

val to_dot : ?pc:tid -> ?dirname:string -> sub term -> int -> tid list -> unit
