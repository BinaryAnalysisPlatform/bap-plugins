open Core_kernel.Std
open Bap.Std
open Color

type t = {
  name : string;
  num_blks : int;
  num_calls : int;
  num_loops : int;
  cyc_comp : int;
}

let sub_profile_with_view
    (module G : Graphlib.Graph with type edge = Graphlib.Tid.Tid.edge and
    type node = tid and type t = Graphlib.Tid.Tid.t) sub =
  let name = Sub.name sub in
  let num_blks = Seq.length (Term.enum blk_t sub) in
  let num_calls = List.length (Util.calls_of_sub sub) in
  let sub_graph = Sub.to_graph sub in
  let edges = G.edges sub_graph in
  let nodes = G.number_of_nodes sub_graph in
  let num_loops = Graphlib.depth_first_search (module G)
      ~enter_edge:(fun k _ n -> if k = `Back then n+1 else n)
      ~init:0 sub_graph in
  let cyc_comp = (Seq.length edges)+nodes+2 in
  {name; num_blks; num_calls; num_loops; cyc_comp}

let sub_profile sub =
  let name = Sub.name sub in
  let num_blks = Seq.length (Term.enum blk_t sub) in
  let num_calls = List.length (Util.calls_of_sub sub) in
  let sub_graph = Sub.to_cfg sub in
  let edges = Graphlib.Ir.edges sub_graph in
  let nodes = Graphlib.Ir.number_of_nodes sub_graph in
  let num_loops = Graphlib.depth_first_search (module Graphlib.Ir)
      ~enter_edge:(fun k _ n -> if k = `Back then n+1 else n)
      ~init:0 sub_graph in
  let cyc_comp = (Seq.length edges)+nodes+2 in
  {name; num_blks; num_calls; num_loops; cyc_comp}

let print_program_profile ?(note="") program =
  Format.printf "PROGRAM:%s\n" note;
  let subs = Term.enum sub_t program in
  Format.printf "\tSubs: %d\n" (Seq.length subs);
  let recursive = Seq.filter subs ~f:Util.calls_self in
  let num_recursive = Seq.length recursive in
  Format.printf "\tNumber of recursive functions: %d\n" num_recursive;
  if num_recursive > 0 then begin
    Format.printf "\t\t[";
    Seq.iter recursive ~f:(fun sub ->
        Format.printf " %s " @@ Sub.name sub);
    Format.printf "]\n"
  end;

  let callgraph = Program.to_graph program in
  let scc_partition = Graphlib.strong_components
      (module Graphlib.Callgraph) callgraph in
  Format.printf "\tMutually recursive: [";
  Seq.iter (Partition.groups scc_partition) ~f:(fun group ->
      let g = Group.enum group in
      if Seq.length g > 1 then begin
        Group.enum group |> Seq.iter ~f:(fun x ->
            Format.printf " %s " @@ Tid.name x);
      end);
  Format.printf "]\n";

  let edges = Graphlib.Callgraph.number_of_edges callgraph in
  let nodes = Graphlib.Callgraph.number_of_nodes callgraph in
  Format.printf "\tCyclomatic complexity: %d\n" (edges+nodes+2);
  Format.print_newline ()

  let print_sub_profile sub =
    Format.printf "SUB: %s\n" @@ Sub.name sub;
    let blks = Term.enum blk_t sub in
    Format.printf "\tBlks: %d\n" (Seq.length blks);
    let calls = Util.calls_of_sub sub in
    Format.printf "\tCalls: %d\n" (List.length calls);
    let sub_graph = Sub.to_cfg sub in
    let edges = Graphlib.Ir.edges sub_graph in
    let nodes = Graphlib.Ir.number_of_nodes sub_graph in
    let back_edges = Graphlib.depth_first_search (module Graphlib.Ir)
        ~enter_edge:(fun k _ n -> if k = `Back then n+1 else n)
        ~init:0 sub_graph in
    Format.printf "\tLoops: %d\n" back_edges;
    Format.printf "\tCyclomatic complexity: %d\n" ((Seq.length edges)+nodes+2);
    Format.print_newline ()

  let callgraph_profile sub_name sub_callgraph =
    let note = sprintf " (ROOTED AT %s)" sub_name in
    (**Format.printf "%a\n" Program.pp sub_callgraph;*)
    print_program_profile ~note sub_callgraph

  let left_justify =
    String.concat_map ~f:(fun c ->
        if c = '\n' then "\\l" else Char.to_string c)

  let output_dot_cfg
      ?(highlight=[]) sub ~filename =
    let open Color in
    let module Cfg = Graphlib.Ir in
    let cfg = Sub.to_cfg sub in
    let string_of_node node =
      sprintf "\"\\%s\"" @@ Blk.to_string @@ Cfg.Node.label node |> left_justify
    in

    let node_attrs node =
      let base =
        if List.Assoc.mem highlight @@ Term.name (Cfg.Node.label node) then
          [`Shape `Box; `Style `Filled; `Fillcolor
             (List.Assoc.find_exn highlight (Term.name (Cfg.Node.label node)))]
        else
          [`Shape `Box; `Style `Filled; `Fillcolor !White] in
      base
    in

    Graphlib.to_dot (module Cfg) ~node_attrs ~string_of_node
      ~filename cfg

  let output_dot_callgraph program ~filename =
    let callgraph = Program.to_graph program in
    let string_of_node n =
      sprintf "%S" @@ Tid.name @@ Graphlib.Callgraph.Node.label n in

    Graphlib.to_dot (module Graphlib.Callgraph) ~string_of_node
      ~filename callgraph

  (** specify node and edge attrs completely beforehand *)
  let output_custom_dot_cfg ~node_attrs ~edge_attrs sub ~filename =

    let open Color in
    let module Cfg = Graphlib.Ir in
    let cfg = Sub.to_cfg sub in
    let string_of_node node =
      sprintf "\"\\%s\"" @@ Blk.to_string @@ Cfg.Node.label node |> left_justify
    in
    Graphlib.to_dot (module Cfg) ~node_attrs ~edge_attrs ~string_of_node
      ~filename cfg

  (** Color order: special overrides path overrides highlight *)
  let output_dot_cfg_path ?(special=[]) ?(highlight=[]) ?(v=false)
      sub path ~filename =
    let module Cfg = Graphlib.Ir in

    let print_path path =
      Format.printf "path:\n";
      Seq.iter path ~f:(fun node ->
          Format.printf "Tid: %a\n" Tid.pp node) in

    let print_edge edge =
      Format.printf "%s -> %s\n"
        (Cfg.Edge.src edge |> Cfg.Node.label |> Term.name )
        (Cfg.Edge.dst edge |> Cfg.Node.label |> Term.name ) in

    if v then print_path path;

    let node_attrs node =
      let blk_name = Term.name (Cfg.Node.label node) in
      if List.Assoc.mem special blk_name then
        [`Shape `Box; `Style `Filled; `Fillcolor
           (List.Assoc.find_exn special blk_name)]
      else if Seq.exists path ~f:(fun tid -> Tid.name tid = blk_name) then
        [`Shape `Box; `Style `Filled; `Fillcolor !Green; `Fontcolor !White]
      else if List.Assoc.mem highlight blk_name then
        [`Shape `Box; `Style `Filled; `Fillcolor
           (List.Assoc.find_exn highlight blk_name)]
      else
        [`Shape `Box; `Style `Filled; `Fillcolor !White]
    in

    let edge_attrs edge =
      if v then print_edge edge;

      let tid_of_node x = Cfg.Node.label x |> Term.name in
      let in_path blk_name =
        Seq.exists path ~f:(fun tid -> Tid.name tid = blk_name)
      in

      let src_tid = Cfg.Edge.src edge |> tid_of_node in
      let dst_tid = Cfg.Edge.dst edge |> tid_of_node in

      (** Gross, but will work. Runs for each edge. *)
      let dst_after_src =
        match Seq.findi path ~f:(fun i tid -> Tid.name tid = src_tid) with
        | Some (i,fsrc_tid) -> (
            match Seq.nth path (i-1) with
            | Some fdst_tid ->
              Tid.name fdst_tid = dst_tid
            | None -> false)
        | None -> false
      in

      let src_in_path = src_tid |> in_path in
      let dst_in_path = dst_tid |> in_path in
      if src_in_path && dst_in_path && dst_after_src then
        [`Color !Green; `Penwidth 4.0;]
      else
        [`Style `Dashed]
    in
    output_custom_dot_cfg sub ~filename ~node_attrs ~edge_attrs
