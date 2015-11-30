open Bap.Std
open Core_kernel.Std

type typ = [`Valid | `Invalid | `Skipped]

let _dir = ref "./analysis/"

let p s i = Format.sprintf s i

let mkdir dir =
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o777

let create_file file =
  let out = open_out_gen [Open_creat; Open_trunc] 0o666 (!_dir^file) in
  Out_channel.close out

(** Create files if they don't exist. If they do exist, truncate them *)
let init dir =
  _dir := dir^"/";
  mkdir !_dir;
  mkdir (!_dir^"cut_groups/");
  mkdir (!_dir^"trim_groups/")

(** Open files for writing and appending (do not truncate). If they do
    not exist, create *)
let append file =
  open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 file

let write file s =
  let out = append file in
  Out_channel.output_string out s;
  Out_channel.close out

let meta s =
  let file = !_dir^"meta.txt" in
  write file s

let misc s =
  let file = !_dir^"misc.txt" in
  write file s

let cuts s =
  let file = !_dir^"cut_groups/cuts.txt" in
  write file s

let trims s =
  let file = !_dir^"trim_groups/trims.txt" in
  write file s

let paths _path_dir s =
  let dir = !_dir^"trim_groups/"^_path_dir in
  mkdir dir;
  let file = dir^"paths.txt" in
  write file s

(** Invalid groups produce no trims (one or more invalid cases). We
    output the graphs for each. This is useful for debug purposes *)
let cut_graph typ src_tid sink_tid sub i j =
  let output dir filename =
    mkdir dir;
    let dummy_path = src_tid ^:: Seq.empty in
    let open Color in
    let special =
      [(Tid.name src_tid, !Blue); (Tid.name sink_tid, !Red)] in
    Profile.output_dot_cfg_path ~special sub dummy_path ~filename in
  match typ with
  | `Invalid -> let dir = (!_dir^"cut_groups/"^(Format.sprintf "invalid_cut_%04d/" i)) in
    let filename = dir^(Format.sprintf "invalid_cut_case_%d_%s_%s.dot"
                          j (Tid.name src_tid) (Tid.name sink_tid)) in
    output dir filename
  | `Valid -> let dir = (!_dir^"cut_groups/"^(Format.sprintf "valid_cut_%04d/" i)) in
    let filename = dir^(Format.sprintf "valid_cut_case_%d_%s_%s.dot"
                          j (Tid.name src_tid) (Tid.name sink_tid)) in
    output dir filename
  | `Skipped -> let dir = (!_dir^"cut_groups/") in
    let filename = dir^(Format.sprintf "skipped_cut_%d.dot" i) in
    output dir filename

(* TODO highlight *)
let trim_graph src_tid sink_tid sub' i j =
  let dir = (!_dir^"trim_groups/"^(Format.sprintf "trim_%04d_case_%04d/" i j)) in
  mkdir dir;
  let filename = Format.sprintf "trim_%s_%s.dot"
      (Tid.name src_tid) (Tid.name sink_tid) in
  let filename = dir^filename in
  let open Color in
  let special = [(Tid.name src_tid, !Blue); (Tid.name sink_tid, !Red)] in
  Profile.output_dot_cfg_path ~special sub' Seq.empty
    ~filename

(* Path with or without analysis, depending on options *)
let path _path_dir number s =
  let dir = !_dir^"trim_groups/"^_path_dir in
  mkdir dir;
  let file = dir^(Format.sprintf "%04d.path" number) in
  write file s

let path_dot _path_dir sub path number =
  let dir = !_dir^"trim_groups/"^_path_dir in
  mkdir dir;
  let filename = dir^(Format.sprintf "%04d.dot" number) in
  Profile.output_dot_cfg_path sub path ~filename

let trim_priority _trim_dir priority =
  let dir = !_dir^"trim_groups/"^_trim_dir in
  mkdir dir;
  let filename = dir^(Format.sprintf "flagged_%04d" priority) in
  write filename ""

let path_priority _path_dir number priority =
  let dir = !_dir^"trim_groups/"^_path_dir in
  mkdir dir;
  let filename = dir^(Format.sprintf "priority_%04d" priority) in
  write filename (Format.sprintf "Path: %04d\n" number)

let output_trim with_dots src_tid sink_tid trim_sub cut_group_id i j profile =
  let open Profile in
  if with_dots then
    trim_graph src_tid sink_tid trim_sub i j;
  trims @@ Format.sprintf "TRIM %d CASE %d\n" i j;
  trims "==================\n";
  let filename = Format.sprintf "trim_%04d_case_%04d/trim_%s_%s.dot"
      i j (Tid.name src_tid) (Tid.name sink_tid) in
  trims @@ Format.sprintf "File: %s\n" filename;
  trims "-----------------------------------\n";
  trims @@
  Format.sprintf "Associated group: %d\n" cut_group_id;
  trims "-----------------------------------\n";
  trims @@ Format.sprintf "SUB: %s\n" profile.name;
  trims @@ Format.sprintf "\tBlks: %d\n" profile.num_blks;
  trims @@ Format.sprintf "\tCalls: %d\n" profile.num_calls;
  trims @@ Format.sprintf "\tLoops: %d\n" profile.num_loops;
  trims @@ Format.sprintf "\tCyclomatic complexity: %d\n\n\n"
    profile.cyc_comp;

  Format.printf "TRIM %d CASE %d\n" i j;
  Format.printf "=================\n";
  Format.printf "File: %s\n" filename;
  Format.printf "-----------------------------------\n";
