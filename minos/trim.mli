(** A trim is a subgraph composed of unique source/sink pairs. One or
    more trims may be produced from a cut group: one trim for each
    unique source/sink pair.
*)

open Bap.Std
open Cut
open Options

type trim = {
  trim_sub : Sub.t;
  cut_group : cut_group;
  src_tid : tid;
  sink_tid : tid
}

(* 'a is debug. 'b is highlight. *)
(* A given cut_group returns a sequence of trims (trim group) *)
val trims : sub term -> cut_group -> 'b -> bool -> trim seq
