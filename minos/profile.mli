open Core_kernel.Std
open Bap.Std

type t = {
  name : string;
  num_blks : int;
  num_calls : int;
  num_loops : int;
  cyc_comp : int;
}

val sub_profile_with_view :
(module Bap.Std.Graphlib.Graph with type edge = Graphlib.Tid.Tid.edge and
type node =  tid and type t = Graphlib.Tid.Tid.t) ->
Sub.t -> t

val sub_profile : Sub.t -> t

val print_sub_profile : Sub.t -> unit

val output_dot_cfg_path: ?special:(string, int) List.Assoc.t ->
?highlight:(string, int) List.Assoc.t ->
?v:bool -> Sub.t -> tid seq -> filename:string -> unit

(*include Printable with type t := t*)
