(** The order of processing is:
    1) Generate cut groups (all pairs of sources and sinks)
    2) Generate trims (valid cut groups where sources occur before sinks)
    3) Generate paths over trims (includes invalid paths that may not end
    with sink)
    4) Produce paths for analysis
*)

type options = {
  check : string; (* the check to use *)
  config : string; (* config file, unused currently *)
  with_dots: bool; (* output dots for trims and cuts *)
  cuts_only: bool; (* produce only cut groups *)
  trims_only: bool; (* produce only trims *)
  path_counts_only: bool; (* perform only path counts *)
  srcs_f : string; (* file containing name of srcs *)
  sinks_f : string; (* file containing name of sinks *)
  single_trim : int; (* consider only a single trim case (needs case) *)
  single_cut : int;  (* consider only a single cut *)
  single_case : int; (* consider only a single case *)
  mem_to_reg : bool; (* perform mem_to_reg *)
  fold_consts : bool; (* perform fold_consts *)
  output_dot_path : bool; (* generate dot outputs for paths *)
  out_dir : string; (* specify an analysis output directory *)
  verbose : bool (* switch for outputting analysis results to terminal *)
}
