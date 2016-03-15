open Bap.Std
open Core_kernel.Std
open Pathlib
open Options
open Simple
open Ctxt
open Check
open Trim
open Graphlib.Std

let (^::) = Seq.cons

let print_count ~v count =
  if v then (Format.printf "%08d%!" count;
             Format.printf "\x08\x08\x08\x08\x08\x08\x08\x08%!")

(** For each direct jmp, if the next block in the sequence is not
    the jmp target, erase this jmp. Sequence must be ordered!! It
    would be wonderful if we could just remove jmp terms that
    correspond to back-edges in the graph. *)
let remove_jmps_path sub path =
  let blk_path = Pathlib.blks_of_path path sub in
  let first_tid = Seq.hd_exn path in
  let first_blk = Seq.hd_exn blk_path ^:: Seq.empty in
  let blk_path',_ =
    (** Goes backward. Last tid is final blk. Starts
        checking second-to-last blk until beginning *)
    Seq.fold ~init:(first_blk,first_tid) (Seq.tl blk_path |> Util.val_exn)
      ~f:(fun (acc,curr) blk ->
          let blk' =
            Term.filter_map jmp_t blk ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Goto (Direct x) ->
                  if Tid.name curr = Tid.name x then Some jmp else None
                | _ -> Some jmp) in
          ((blk'^::acc),(Util.tid_of_blk blk'))) in
  blk_path'

let sub_of_blk_path path =
  let builder = Sub.Builder.create () in
  Seq.iter path ~f:(fun blk ->
      Sub.Builder.add_blk builder blk);
  Sub.Builder.result builder

let dot_cb filename_prefix sub path count =
  let filename = "./"^filename_prefix^(Format.sprintf "%04d.dot" count) in
  Profile.output_dot_cfg_path sub path ~filename

let warn_invalid path (ctxt : Ctxt.t) =
  dot_cb "invalid_path_" ctxt.trim.trim_sub path ctxt.count; (* Debug *)
  (* debug path ctxt;*) (* Debug dot *)
  Format.printf "Pre-empt! Skipping path %d, sink tid does not match.%!\n"
    ctxt.count

let consumer_ctxt_of_ctxt (ctxt : Ctxt.t) sub_path : Check.ctxt =
  let open Check in
  {sub_path = sub_path;
   num_paths = 0; (** used for should_produce *)
   path_dir = ctxt.path_dir;
   options = ctxt.options;
   project = ctxt.project;
   trim = ctxt.trim;
   trim_dir = ctxt.trim_dir;
   count = ctxt.count}

let producer_ctxt_of_ctxt (ctxt : Ctxt.t) num_paths : Check.ctxt =
  let open Check in
  {sub_path = Sub.create (); (** doesnt matter *)
   num_paths;
   path_dir = ctxt.path_dir;
   trim_dir = ctxt.trim_dir;
   options = ctxt.options;
   project = ctxt.project;
   trim = ctxt.trim;
   count = 0} (** doesn't matter *)

let process_valid path (ctxt : Ctxt.t) =
  let open Check in
  let blk_path = remove_jmps_path ctxt.trim.trim_sub path in
  let sub_path = sub_of_blk_path blk_path in
  let o = ctxt.options in
  let output_dot prefix = Output.path_dot
      (ctxt.path_dir^prefix) ctxt.trim.trim_sub path ctxt.count in
  let process () =
    try
      let sub_path' = Util.make_call_returns_explicit sub_path in
      let sub_path'' = Simple.simplify ctxt sub_path' in
      let check_ctxt = consumer_ctxt_of_ctxt ctxt sub_path'' in
      Path_consumer.consume sub_path'' ctxt.check check_ctxt
    with
    | Not_found ->
      output_dot "path_no_simple_";
      Format.printf "Skipping this path: could not simplify\n%!" in

  match (o.path_counts_only,o.output_dot_path) with
  | (true,s) ->
    print_count ~v:true ctxt.count;
    if s then output_dot "";
    Output.path ctxt.path_dir
      ctxt.count (Sub.to_string sub_path)
  | (false,s) ->
    process ();
    print_count ~v:true ctxt.count;
    if s then output_dot ""

let analyze path (ctxt : Ctxt.t) =
  (** Pre-empt processing if the last tid in this path is not the
      sink. This should not happen. *)
  if ctxt.check.reverse then
    let path = Seq.to_list_rev path |> Seq.of_list in
    if (Seq.hd_exn path) <> ctxt.trim.sink_tid then
      warn_invalid path ctxt
    else
      process_valid path ctxt

let already_seen seen blk_tid =
  Seq.exists seen ~f:(fun tid ->
      if blk_tid = tid then true else false)

let process_path local_state blk_tid exit_type (ctxt : Ctxt.t) =
  let current_path,depth = local_state in
  let path = blk_tid ^:: current_path in
  analyze path ctxt;
  (*dot_cb !sub path ctxt.count;*)
  {ctxt with count = ctxt.count + 1}

let finish local_state blk_tid (ctxt : Ctxt.t) =
  process_path local_state blk_tid `Terminal ctxt

let f local_state blk_tid continue (ctxt : Ctxt.t) =
  if ctxt.sample = ctxt.count then ctxt else (** terminate path if sample hit *)
    let current_path,depth = local_state in
    let do_process_path exit_typ =
      process_path local_state blk_tid exit_typ ctxt in
    if already_seen current_path blk_tid then
      do_process_path `Seen
    else if depth = ctxt.max_depth then
      do_process_path `Max_depth
    else
      let state = (blk_tid ^:: current_path, depth+1) in
      continue ~state

let debug_hide_edges e =
  Format.printf "Added edge %s\n%!" @@  Tid.name @@
  Graphs.Tid.Edge.label e

(** Find all back edges in the graph. Collect all edge srcs (nodes)
    that are in the sub. Skip these nodes in the new graph.*)
let hide_back_edges sub graph_module =
  let module G = Graphs.Tid in
  let edge_set = G.Edge.Hash_set.create () in
  Graphlib.depth_first_search (module G)
    ~enter_edge:(fun k e _ ->
        if k = `Back then
          (* debug_hide_edges e*)
          Hash_set.add edge_set e
        else
          ())
    ~init:() (Sub.to_graph sub);
  Graphlib.filtered graph_module
    ~skip_edge:(fun e -> Hash_set.mem edge_set e) ()

let debug_hide_non_existing node =
  Format.printf "Skipping %s\n" @@ Tid.name node

(** Find all edges in the sub. Collect all edge dsts (nodes) that
    are not in the sub. Skip these nodes in the new graph. *)
let hide_non_existing sub graph_module =
  let module G = Graphs.Tid in
  let g = Sub.to_graph sub in
  let edges = G.edges g in
  let nodes = G.Node.Hash_set.create () in
  Seq.fold ~init:() edges ~f:(fun acc edge ->
      let dst = G.Edge.dst edge in
      match Term.find blk_t sub dst with
      | Some blk -> ()
      | None ->
        Hash_set.add nodes dst);
  Graphlib.filtered graph_module ~skip_node:(fun node ->
      (* debug_hide_non_existing node *)
      Hash_set.mem nodes node) ()

let debug_reachable reachable_src reachable_sink =
  Format.printf "Reachable src:\n%!\n";
  Set.iter reachable_src ~f:(fun x ->
      Format.printf "%s |" @@ Tid.name @@ x);
  Format.printf "\n";
  Format.printf "Reachable sink:\n%!\n";
  Set.iter reachable_sink ~f:(fun x ->
      Format.printf "%s |" @@ Tid.name @@x);
  Format.printf "\n"

(** After removing edges, perform reachable *)
let trim_reachable trim graph_module =
  let module G = Graphs.Tid in
  let reachable_src =
    Graphlib.fold_reachable graph_module
      ~init:G.Node.Set.empty ~f:Set.add
      (Sub.to_graph trim.trim_sub) trim.src_tid in
  let reachable_sink =
    Graphlib.fold_reachable graph_module
      ~rev:true ~init:G.Node.Set.empty ~f:Set.add
      (Sub.to_graph trim.trim_sub) trim.sink_tid in
  (*debug_reachable reachable_src reachable_sink;*)
  let nodes =
    Set.inter reachable_src reachable_sink in
  let nodes_set = G.Node.Hash_set.create () in
  Set.iter nodes ~f:(fun x -> Hash_set.add nodes_set x);
  Graphlib.filtered graph_module
    ~skip_node:(fun node -> not (Hash_set.mem nodes_set node)) ()

let debug filename graph_view graph =
  let string_of_edge edge =
    Tid.name @@ Graphs.Tid.Edge.label edge in
  let string_of_node node =
    sprintf "\"\\%s\"" @@ Tid.to_string node
  in
  Graphlib.to_dot
    ~string_of_edge
    ~string_of_node graph_view
    ~filename
    graph

(** Remove jmps in a block that do not have jump targets in the sub *)
let kill_non_existing_jmps sub =
  Term.map blk_t sub ~f:(fun blk ->
      Term.filter jmp_t blk ~f:(fun jmp ->
          match Jmp.kind jmp with
          | Goto (Direct target_tid) ->
            (match Term.find blk_t sub target_tid with
             | Some blk -> true
             | None -> false)
          | _ -> true))

(** Add all the nodes in the filtered graph view *)
let view_to_sub graph_view graph sub : Sub.t =
  let builder = Sub.Builder.create ~name:("minimal_"^(Sub.name sub)) () in
  Graphlib.depth_first_search graph_view ~enter_node:(fun i blk_tid s ->
      match Term.find blk_t sub blk_tid with
      | Some blk -> Sub.Builder.add_blk builder blk
      | None -> ())
    graph ~init:();
  Sub.Builder.result builder

let produce project options path_dir trim_dir trim check =
  let module G = Graphs.Tid in
  let sub = trim.trim_sub in
  let graph = Sub.to_graph sub in
  debug "normal_graph_view.dot" (module G) graph;

  (** First create a graph view from which we will generate paths.
      1) Hide back edges (loops), creating a dag
      2) For the edges left over, if they point to any node outside
      of our trim, remove those. This step may be unnecessary, since our trim
      shouldn't contain nodes that are not reachable.
      3) Because the removal of back edges can cause the sink to be not
      reachable any more, perform reachable again on the original trim. *)
  let filtered_graph =
    hide_back_edges sub (module G) |>
    hide_non_existing sub |>
    trim_reachable trim in
  (** debug "filtered_graph_view.dot" filtered_graph graph;*)

  (** 1) Create a sub from all nodes in the filtered graph (in case any were
      removed by that process)
      2) Remove all jumps if the jump target does not exist in the sub--this
      syncs the sub with the hide_non_existing operation on the graph.
      3) Use this as the underlying representation of terms of the graph that we
      will find paths over. Note that back-edges/jmps are not removed in this
      underlying representation (and they don't need to be). Such back edges/jmp
      terms are removed on a per-path basis.
  *)
  let sub' = view_to_sub filtered_graph graph sub |> kill_non_existing_jmps in

  let module G' = (val filtered_graph : Graphlib.Graph with
                    type edge = Graphs.Tid.edge and
                  type node = tid and
                  type t = Graphs.Tid.t) in

  (** Finally, create the context *)
  let trim = {trim with trim_sub = sub'} in

  let init_local_state = (Seq.empty, 0) in
  let init_ctxt =
    {path_dir; count = 0;
     project;
     options;
     trim;
     max_depth = check.max_depth;
     g = filtered_graph;
     check;
     trim_dir;
     sample = check.sample} in

  let sub_graph = Sub.to_graph init_ctxt.trim.trim_sub in
  let num_paths = Util.num_paths_dag filtered_graph sub_graph trim.src_tid in

  Format.printf "NUM PATHS: %d\n%!" num_paths;
  Output.paths trim_dir
    (Format.sprintf "Total possible paths: %d\n%!" num_paths);

  (** Debug *)
  (*
  let res = Profile.sub_profile_with_view filtered_graph init_ctxt.trim.trim_sub in
  Output.output_trim init_ctxt.trim.src_tid init_ctxt.trim.sink_tid
    init_ctxt.trim.trim_sub init_ctxt.trim.cut_group.id 0 0 res;
  *)

  let check_ctxt = producer_ctxt_of_ctxt init_ctxt num_paths in

  if options.path_counts_only then
    init_ctxt
  else if Seq.length (G'.nodes sub_graph) = 0 then
    (Format.printf "Not producing, sink no longer reachable via source\n%!";
     init_ctxt)
  else if check.should_produce check_ctxt || init_ctxt.sample > 0 then
    Pathlib.fold_paths_graph
      ~rev:check.reverse
      filtered_graph
      ~sub:sub_graph
      ~state:init_local_state
      ~acc:init_ctxt
      ~blk:(if check.reverse then trim.sink_tid else trim.src_tid)
      ~finish
      ~f
  else
    (Format.printf "Not producing, 'should_produce' false\n%!";
     init_ctxt)
