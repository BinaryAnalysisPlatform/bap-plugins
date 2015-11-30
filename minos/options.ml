type options = {
  check : string;
  config : string;
  with_dots: bool;
  cuts_only: bool;
  trims_only: bool;
  path_counts_only: bool;
  srcs_f : string;
  sinks_f : string;
  single_trim : int;
  single_cut : int;
  single_case : int;
  mem_to_reg : bool;
  fold_consts : bool;
  output_dot_path : bool;
  out_dir : string;
  verbose : bool
}
