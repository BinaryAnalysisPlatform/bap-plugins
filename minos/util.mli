open Bap.Std
open Core_kernel

exception Timeout

(** Find a section by name, e.g. ".rodata" *)
val find_section_by_name : project -> string -> mem option

(** This just prints the difference between non-constant folded
    expressions and folded expressions. Does not update *)
val print_constant_fold : project -> unit -> unit

(** Expects '@' in sub name *)
val sub_from_name : project -> string -> sub term option

(** Gets the subroutine name of a jmp target *)
val resolve_jmp_name : Jmp.t -> string option

(** Given a blk, return the tid successors *)
val succs_of_blk : sub term -> blk term -> tid seq

(** Given a blk_tid, return the tid successors *)
val succs_of_blk_tid : sub term -> tid -> tid seq

val blk_of_tid : sub term -> tid -> blk term

val tid_of_blk : blk term -> tid

val tid_of_sub : sub term -> tid

val val_exn : 'a option -> 'a

val calls_of_blk : blk term -> call list

val calls_of_blk_str : blk term -> string list

val calls_of_sub : sub term -> tid list

val get_exit_blocks : sub term -> blk term seq

val is_exit_block : sub term -> blk term -> bool

val contains_call : blk term -> string -> bool

val blks_with_calls : sub term -> string list

val contains_indirect_goto: blk term -> bool

val ends_with_lr : blk term -> bool

val calls_of_blk_with_tid : blk term -> (tid * call) list

val target_tid_of_call :call -> tid option

val sub_of_tid : project -> tid -> sub term option

val get_return_target : call -> tid option

val deep_clone_sub : sub term -> sub term * tid Tid.Table.t

val calls_self : sub term -> bool

val callgraph_of_sub : project -> tid -> program term

val is_mutually_recursive : Program.t -> bool

val make_exported_calls_explicit : project -> project

val make_implicit_jmp_conds_explicit : project -> project

(** Insert R0 or RAX after a call for a given sub *)
val make_call_returns_explicit : Sub.t -> Sub.t

(** Find number of paths in a dag. Uses DP on dfs. MUST be a DAG with
    a single exit node. Undefined behavior for multiple exit nodes. *)
val num_paths_dag :
(module Bap.Std.Graphlib.Graph with type edge = Graphlib.Tid.Tid.edge and
type node = tid and type t = Graphlib.Tid.Tid.t) ->
 Graphlib.Tid.Tid.t -> tid -> int

val timeout : secs:int -> f:('a -> 'b) -> x:'a -> 'b
