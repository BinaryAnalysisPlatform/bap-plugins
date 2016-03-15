open Core_kernel.Std
open Bap.Std
open Graphlib.Std

type t = {
  name : string;
  num_blks : int;
  num_calls : int;
  num_loops : int;
  cyc_comp : int;
}

val sub_profile_with_view :
  (module Graphlib.Graph with type edge = Graphs.Tid.edge and
  type node =  tid and type t = Graphs.Tid.t) ->
  Sub.t -> t

val sub_profile : Sub.t -> t

val print_sub_profile : Sub.t -> unit

val output_dot_cfg_path: ?special:(string, int) List.Assoc.t ->
  ?highlight:(string, int) List.Assoc.t ->
  ?v:bool -> Sub.t -> tid seq -> filename:string -> unit
