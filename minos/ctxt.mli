(** Global path state/context, needs renaming *)

open Bap.Std
open Options
open Trim
open Graphlib.Std

type t = {
  check : Check.t;
  path_dir: string;
  trim_dir : string;
  count : int;
  project : project;
  options: options;
  trim : trim;
  max_depth : int; (** depth to go to before terminating path *)
  sample: int; (** number of paths to sample *)
  g : (module Graph with type edge = Graphs.Tid.edge and
      type node = tid and type t = Graphs.Tid.t)
}
