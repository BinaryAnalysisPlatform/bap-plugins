open Bap.Std
open Options
open Trim

(* This is a ctxt type for check *)
type ctxt = {
    sub_path : Sub.t;
    num_paths : int;
    path_dir : string;
    options : options;
    project : project;
    trim : trim;
    trim_dir : string;
    count : int; (* Current count *)
}

type t = {
  should_produce : (ctxt -> bool);
  run: (ctxt -> int);
  reverse : bool; (* reverse the direction from sink to source *)
  max_depth : int; (* depth to which to traverse *)
  sample : int; (* number of paths to sample *)
  timeout : int
}
