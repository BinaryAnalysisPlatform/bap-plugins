open Core_kernel.Std
open Bap.Std
open Options

module Cmdline = struct
  open Cmdliner

  let check : string option Term.t =
    let doc = "The check that should be run" in
    Arg.(value & opt (some string) None &
         info ["check"] ~doc)

  let config : string option Term.t =
    let doc = "Config file for source sink analysis" in
    Arg.(value & opt (some string) None &
         info ["config"] ~doc)

  let srcs_f : string option Term.t =
    let doc = "File containing sources per newline" in
    Arg.(value & opt (some string) None &
         info ["srcs"] ~doc)

  let sinks_f : string option Term.t =
    let doc = "File containing sinks per newline" in
    Arg.(value & opt (some string) None &
         info ["sinks"] ~doc)

  let with_dots : bool Term.t =
    let doc = "Produce dot outputs of cuts and trims" in
    Arg.(value & flag & info ["with_dots"] ~doc)

  let cuts_only : bool Term.t =
    let doc = "Only calculate the cut groups between a src and sink: the \
               lowest common ancestor in the callstrings of src and sink \
               to root" in
    Arg.(value & flag & info ["cuts_only"] ~doc)

  let trims_only : bool Term.t =
    let doc = "Calculate the cuts and then trim the cuts between src and sink. \
               Turns on [cuts_only]" in
    Arg.(value & flag & info ["trims_only"] ~doc)

  let path_counts_only : bool Term.t =
    let doc = "Print out a path counter for each unique src sink pair trim. \
               Turns on [cuts_only] and [trims_only]" in
    Arg.(value & flag & info ["path_counts_only"] ~doc)

  let single_case : int option Term.t =
    let doc = "Process only this case for paths analysis." in
    Arg.(value & opt (some int) None &
         info ["single_case"] ~doc)

  let single_trim : int option Term.t =
    let doc = "Process only this trim for paths analysis." in
    Arg.(value & opt (some int) None &
         info ["single_trim"] ~doc)

  let single_cut : int option Term.t =
    let doc = "Process only this cut for path analysis." in
    Arg.(value & opt (some int) None &
         info ["single_cut"] ~doc)

  let mem_to_reg : bool Term.t =
    let doc = "Promote memory to registers in the path" in
    Arg.(value & flag & info ["mem_to_reg"] ~doc)

  let fold_consts : bool Term.t =
    let doc = "Fold constants in the path" in
    Arg.(value & flag & info ["fold_consts"] ~doc)

  let output_dot_path : bool Term.t =
    let doc = "Output dot files for each path" in
    Arg.(value & flag & info ["output_dot_path"] ~doc)

  let out_dir : string option Term.t =
    let doc = "Analysis output directory" in
    Arg.(value & opt (some string) None &
         info ["out_dir"] ~doc)

  let verbose : bool Term.t =
    let doc = "Verbose output (intended for analysis)" in
    Arg.(value & flag & info ["verbose"] ~doc)

  let info =
    Term.info ~doc:"Minos" "Minos"

  let process_args check config srcs_f sinks_f with_dots cuts_only
      trims_only path_counts_only single_trim single_cut single_case
      mem_to_reg fold_consts output_dot_path out_dir verbose =
    let (!) opt default = Option.value opt ~default in
    let check = !check "" in
    let config = !config "" in
    let srcs_f = !srcs_f "" in
    let sinks_f = !sinks_f "" in
    let single_case = !single_case (-1) in
    let single_trim = !single_trim (-1) in
    let single_cut = !single_cut (-1) in
    let out_dir = !out_dir "./analysis/" in
    { check; config; with_dots; cuts_only;
      trims_only; path_counts_only;
      srcs_f; sinks_f; single_trim;
      single_cut; single_case;
      mem_to_reg; fold_consts;
      output_dot_path; out_dir;
      verbose}

  let parse argv =
    match Term.eval ~argv
            (Term.(pure process_args
                   $check
                   $config
                   $srcs_f
                   $sinks_f
                   $with_dots
                   $cuts_only
                   $trims_only
                   $path_counts_only
                   $single_trim
                   $single_cut
                   $single_case
                   $mem_to_reg
                   $fold_consts
                   $output_dot_path
                   $out_dir
                   $verbose),info)
    with
    | `Ok opts -> opts
    | _ -> exit 1
end

module Plugin (E : sig val project : project val options : options end) = struct
  open E

  let check () =
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
    (match options.check with
     | "" -> Format.printf "~Check selected: NONE\n"
     | _ ->  Format.printf "~Check selected: %s\n" options.check);
    Format.printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
    Check_suite.select options.check

  let parse_src_config src =
    let open Cut in
    match String.split ~on:':' src with
    (* Inline from LCA *)
    | src::[] -> {src_at_root = false; src_at_nth = 0; src}
    (* Set LCA to ROOT or some sub *)
    | "LCA"::sub::[] ->
      (match sub with
       | "ROOT" -> {src_at_root = true; src_at_nth = 0; src}
       | nth -> {src_at_root = false; src_at_nth = Int.of_string nth; src})
    | _ -> failwith "Please specify a source. Formats:\
                     LCA:[ ROOT | [1..]]"

  let produce_trims cut_groups project filter =
    let open Cut in (* resolve g.lca *)
    let open Trim in
    let trim_groups =
      Seq.foldi ~init:Seq.empty cut_groups ~f:(fun i acc g ->
          let process_cut () =
            (* 2. Inline *)
            (* When we inline and cut for a particular group, we are
               only interested in tracking the src and sink caller
               blks *)

            let sub',highlight =
              if g.depth = 0 then (g.lca_sub,[]) else
                let warn = true in
                Interprocedural.inline_n ~warn project g.lca_sub filter
                  (g.depth) in

            (* 3. Trim everything in the inlined CFG that does not start
               at src and end at sink *)
            let trim_group = Trim.trims sub' g highlight options.with_dots in
            match Seq.length trim_group with
            | 0 ->
              Output.meta @@
              Format.sprintf "Group dropped (source after sink): %d\n" g.id;
              acc
            | _ -> (trim_group ^:: acc) in

          match options.single_cut with
          | -1 -> process_cut ()
          | x when x = g.id -> process_cut ()
          | _ -> acc) in
    trim_groups

  let process_paths trim_groups project =
    let open Ctxt in
    let process_case case x y i j =
      Format.printf "-=-=-PROCESSING-=-=\n";
      Format.printf "TRIM %d CASE %d paths counter:\n" x y;
      Format.printf "----------------------\n";

      let trim_dir = Format.sprintf "trim_%04d_case_%04d/" i j in
      let path_dir = Format.sprintf "trim_%04d_case_%04d/paths/" i j in

      let glob = Path_producer.produce project
          options path_dir trim_dir case (check ()) in

      Format.printf "%08d%!\n" glob.count;

      Output.paths trim_dir
        (Format.sprintf "Paths: %d\n" glob.count) in

    Seq.iteri trim_groups ~f:(fun i trim_group ->
        Seq.iteri trim_group ~f:(fun j case ->
            match (options.single_trim,options.single_case) with
            | (-1,-1) -> process_case case i j i j
            | (x,_) when x = i -> process_case case x j i j
            | (x,y) when x = i && y = j -> process_case case x y i j
            | (_,_) -> ()))

  let output_cut_groups cut_groups =
    Format.printf "[+] %d cut groups\n" @@ Seq.length cut_groups;
    Output.meta @@ Format.sprintf "Cut groups: %d\n\n" (Seq.length cut_groups);
    Seq.iteri cut_groups ~f:(fun i g ->
        (** To stdout *)
        Cut.print_cut_group g;
        (** To file *)
        Cut.output_cut_group g)

  let output_trim_groups trim_groups =
    let open Cut in
    let open Trim in
    let output_case i j case =
      let profile = Profile.sub_profile case.trim_sub in
      (** To stdout *)
      Profile.print_sub_profile case.trim_sub;
      (** To file *)
      Output.output_trim options.with_dots case.src_tid case.sink_tid
        case.trim_sub case.cut_group.id i j profile in

    Seq.iteri trim_groups ~f:(fun i trim_group ->
        Seq.iteri trim_group ~f:(fun j case ->
            match (options.single_trim,options.single_case) with
            | (-1,-1) -> output_case i j case
            | (x,_) when x = i -> output_case i j case
            | (x,y) when x = i && y = j -> output_case i j case
            | (_,_) -> ()))

    let main () =
      Output.init options.out_dir;
      let project = Util.make_exported_calls_explicit project in
      (** Perform cond negation *)
      let project' = Util.make_implicit_jmp_conds_explicit project in

      let callgraph = Program.to_graph (Project.program project') in
      (* 0. Read srcs and sinks *)
      let srcs = In_channel.read_lines options.srcs_f in
      let sinks = In_channel.read_lines options.sinks_f in
      let src = List.hd_exn srcs in (* only one pair for now *)
      let sink = List.hd_exn sinks in

      Output.meta @@ Format.sprintf "Src: %s\n" src;
      Output.meta @@ Format.sprintf "Sink: %s\n\n" sink;

      (** if src_is_entry, the source block will NOT be the block that
          calls some source sub, but the entry block of the sub
          specified (if possible). If it is ROOT:[sub], then we will
          inline from the root of the sink callstring *)
      let src_config = parse_src_config src in

      let filter = Filter.cpp_filter ~extra:[src; sink] in

      (* 1. Cut out groups of source/sinks in the callgraph. Inclues lca
         of src and sink *)
      Format.printf "Producing cut groups...\n\n%!";
      let cut_groups = Cut.cuts project' callgraph src_config sink in
      output_cut_groups cut_groups;

      if options.cuts_only then exit 0;

      (* 2. Trims *)
      Format.printf "Producing trims...\n\n%!";
      let trim_groups = produce_trims cut_groups project filter in
      output_trim_groups trim_groups;

      if options.trims_only then exit 0;

      (* 3. paths *)
      Format.printf "Processing paths...\n\n%!";
      process_paths trim_groups project;
end

let run project options =
  let module Main = Plugin(struct
      let project = project
      let options = options
    end) in
  Main.main ()

let main argv project =
  let options = Cmdline.parse argv in
  run project options

let () = Project.register_pass_with_args' "minos" main
