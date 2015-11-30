open Bap.Std
open Core_kernel.Std
open Profile

(* Output meta information of analysis for cut groups *)
(* `Valid groups have source before sink.
   `Invalid groups have source after sink.
   `Skipped groups have multiple block structures matching
   the sink. We skip these for now *)
type typ = [`Valid | `Invalid | `Skipped]

(* With root directory *)
val init : string -> unit

(* Output to meta.txt *)
val meta : string -> unit

(* Output to misc.txt *)
val misc : string -> unit

(* Output to cuts.txt *)
val cuts : string -> unit

(* Output the graphs of a cut group, and create directories depending
   on valid or invalid *)
val cut_graph: typ -> tid -> tid -> Sub.t -> int -> int -> unit

(* Output to trims.txt *)
val trims : string -> unit

(* Output a trim group's cases and graphs *)
val trim_graph : tid -> tid -> Sub.t -> int -> int -> unit

(* Output to paths.txt *)
val paths : string -> string -> unit

(* Output the path trace (with data dependence *)
val path : string -> int -> string -> unit

(* Output the .dot of the path trace *)
val path_dot : string -> Sub.t -> tid seq -> int -> unit

(* Output the priority for checking a path *)
val path_priority : string -> int -> int -> unit

(* Output trim priority *)
val trim_priority : string -> int -> unit

(* Output bulk trim information *)
val output_trim : bool -> tid -> tid -> Sub.t -> int -> int -> int -> Profile.t -> unit
