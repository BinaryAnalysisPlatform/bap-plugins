open Bap.Std
open Core_kernel.Std

type src_config =
  {src_at_root : bool;
   src_at_nth : int;
   src : string;
  }

type cut_group = {
  src_caller_blks : Blk.t seq;
  sink_caller_blks : Blk.t seq;
  src_callstring : tid seq;
  sink_callstring : tid seq;
  src_caller_sub : Sub.t;
  sink_caller_sub : Sub.t;
  lca_sub : Sub.t;
  lca_name : string;
  depth: int;
  id: int;
}

let already_cut = Tid.Set.empty

let rev_seq s = Seq.to_list_rev s |> Seq.of_list

let print_callstring ~v path =
  if v then
    (List.iter (Seq.to_list path |> List.rev) ~f:(fun tid ->
         if v then
           Format.printf "|%s|%!" @@ Tid.name tid);
     Format.printf "\n%!")

let print_callstrings ~v paths =
  if v then
    (Seq.iter paths ~f:(fun path -> print_callstring ~v:true path);
     Format.printf "\n")

let output_callstring f path =
    (List.iter (Seq.to_list path |> List.rev) ~f:(fun tid ->
           f @@ Format.sprintf "|%s|" @@ Tid.name tid);
     f "\n")

let output_callstrings f paths =
    (Seq.iter paths ~f:(fun path -> output_callstring f path);
     f "\n")

let callstrings_from_x callgraph x =
  let callstrings,_ =
    Callstrings.do_path_search
      ~v:false ~rev:true x callgraph (-1) in
  callstrings

let callstrings project sub callgraph =
  match Util.sub_from_name project sub with
  | Some sub ->
    callstrings_from_x callgraph sub
  | None ->
    Seq.empty

let print_cut_group g =
  Format.printf "\n---------LCA---------\n";
  Format.printf "%s" g.lca_name;
  Format.printf "\n-----------------------\n";
  Format.printf "\n--Pair callstrings:--\n";
  Format.printf "  ";
  print_callstring ~v:true g.src_callstring;
  Format.printf "  ";
  print_callstring ~v:true g.sink_callstring;

  Format.printf "Caller of src:\n";
  Format.printf "  %s\n" @@ Sub.name g.src_caller_sub;
  Format.printf "  #Blks that call:\n  ";
  Format.printf "  %d\n    " @@ Seq.length g.src_caller_blks;
  Seq.iter g.src_caller_blks ~f:(fun blk ->
      Format.printf "|%s| " @@ Tid.name (Term.tid blk));
  Format.print_newline ();

  Format.printf "Caller of sink:\n";
  Format.printf "  %s\n" @@ Sub.name g.sink_caller_sub;
  Format.printf "  #Blks that call:\n  ";
  Format.printf "  %d\n    " @@ Seq.length g.sink_caller_blks;
  Seq.iter g.sink_caller_blks ~f:(fun blk ->
      Format.printf "|%s| " @@ Tid.name (Term.tid blk));
  Format.print_newline ();
  Format.printf "Depth: %d\n" g.depth;
  Format.print_newline ()

let output_cut_group g =
  Output.cuts @@ Format.sprintf "\n|||GROUP %d|||\n" g.id;
  Output.cuts "----------------LCA----------------\n";
  Output.cuts g.lca_name;
  Output.cuts "\n---------------------------------\n";
  Output.cuts "\n--Pair callstrings:--\n";
  Output.cuts "  ";
  output_callstring Output.cuts g.src_callstring;
  Output.cuts "  ";
  output_callstring Output.cuts g.sink_callstring;

  Output.cuts "Caller of src:\n";
  Output.cuts @@ Format.sprintf "  %s\n" @@ Sub.name g.src_caller_sub;
  Output.cuts "  #Blks that call:\n  ";
  Output.cuts @@ Format.sprintf "  %d\n    " @@ Seq.length g.src_caller_blks;
  let res =
    List.fold_right ~init:"" (Seq.to_list g.src_caller_blks) ~f:(fun blk acc ->
        (Format.sprintf "|%s| " @@ Tid.name (Term.tid blk)) ^ acc ) in
  Output.cuts (res^"\n");

  Output.cuts "Caller of sink:\n";
  Output.cuts @@ Format.sprintf "  %s\n" @@ Sub.name g.sink_caller_sub;
  Output.cuts "  #Blks that call:\n  ";
  Output.cuts @@ Format.sprintf "  %d\n    " @@ Seq.length g.sink_caller_blks;
  let res =
    List.fold_right ~init:"" (Seq.to_list g.sink_caller_blks) ~f:(fun blk acc ->
        (Format.sprintf "|%s| " @@ Tid.name (Term.tid blk)) ^ acc ) in
  Output.cuts (res^"\n");
  Output.cuts "\n";
  Output.cuts @@ Format.sprintf "Depth: %d\n" g.depth;
  Output.cuts "\n"

let make_cut ~nth project lca_tid sink_cs sink id =
  let (!) = Seq.to_list_rev in

  let blk_tids_of_caller tid x =
    Term.enum blk_t (Util.sub_of_tid project tid |> Util.val_exn)
    |> Seq.filter ~f:(fun blk -> Util.contains_call blk x)
    |> Seq.map ~f:Util.tid_of_blk in

  let blks_of_caller sub =
    Seq.map ~f:(fun tid -> Util.blk_of_tid sub tid) in

  (* debug
  let sink_caller_sub_tid = List.nth_exn !sink_cs 0 in
  Format.printf "0 Sink caller sub: %s\n%!" @@ Tid.name sink_caller_sub_tid;*)
  let sink_caller_sub_tid = List.nth_exn !sink_cs 1 in
  (*Format.printf "1 Sink caller sub: %s\n%!" @@ Tid.name sink_caller_sub_tid;*)

  (*failwith "die" |> ignore;*)

  let src_callstring = lca_tid ^:: Seq.empty in
  let sink_callstring = sink_cs in
  let lca_sub = Util.sub_of_tid project lca_tid |> Util.val_exn in
  let lca_name = Sub.name lca_sub in
  let src_caller_sub = lca_sub in
  let src_caller_blks =
    (Util.sub_of_tid project lca_tid
     |> Util.val_exn
     |> Term.first blk_t
     |> Util.val_exn) ^:: Seq.empty in
  let sink_caller_sub =
    Util.sub_of_tid project sink_caller_sub_tid |> Util.val_exn in
  let sink_caller_blks =
    blk_tids_of_caller sink_caller_sub_tid sink
    |> blks_of_caller sink_caller_sub in

  let depth =
    match nth with
    | -1 -> Seq.length sink_cs
    | nth -> nth in

  let group =
    {src_caller_blks;
     sink_caller_blks;
     src_callstring;
     sink_callstring;
     src_caller_sub;
     sink_caller_sub;
     lca_name; lca_sub;
     depth; id} in
  group

let lca_root_of_sink project sink callgraph : cut_group seq =
  Format.printf "===================================\n";
  Format.printf "ROOTING at top of sink callstrings.\n";
  Format.printf "===================================\n";
  Format.printf "callstring sinks:\n";
  let callstrings_sinks = callstrings project sink callgraph in
  print_callstrings ~v:true callstrings_sinks;

  (** No need to remove duplicates, should be uniqe *)
  Seq.foldi ~init:Seq.empty callstrings_sinks ~f:(fun i acc sink_cs ->
      let lca_tid = Seq.hd_exn sink_cs in
      (* nth:-1 means process the entire length *)
      let group = make_cut ~nth:(-1) project lca_tid sink_cs sink i in
      group ^:: acc)

let lca_nth_of_sink project nth sink callgraph =
  Format.printf "===================================\n";
  Format.printf "ROOTING at NTH %d sink callstrings.\n" nth;
  Format.printf "===================================\n";
  Format.printf "callstring sinks:\n";
  let callstrings_sinks = callstrings project sink callgraph in
  print_callstrings ~v:true callstrings_sinks;
  Output.meta "callstring sinks:\n";
  output_callstrings Output.meta callstrings_sinks;

  (** Callstring order: @foo @bar @memcpy *)

  (** Take the callstring from the sink up to nth *)
  let callstrings_sinks =
    Seq.map callstrings_sinks ~f:(fun callstring ->
        let rev_list =
          Seq.to_list_rev callstring in
        List.take rev_list (nth+1)) |> Seq.map ~f:(fun x ->
        x |> List.rev |> Seq.of_list) in

  (** Callstring order: @foo @bar @memcpy *)

  let callstrings_sinks_l =
    Seq.fold ~init:[] callstrings_sinks ~f:(fun acc callstring ->
        let res = Seq.fold ~init:[] callstring ~f:(fun acc tid ->
            tid :: acc) in
        res :: acc) in

  (** Callstring order: @memcpy @foo @bar *)

  let callstrings_sinks = List.dedup callstrings_sinks_l |>
                          List.map ~f:Seq.of_list |> Seq.of_list in

  (** Callstring order: @foo @bar @memcpy *)
  Seq.foldi ~init:Seq.empty callstrings_sinks ~f:(fun i acc sink_cs ->
      let l = Seq.to_list sink_cs in
      match List.nth l nth with
      | Some lca_tid ->
        let s = Format.sprintf "lca: %s\n%!" @@ Tid.name lca_tid in
        Format.printf "%s" s;
        Output.meta s;
        (* We have to reverse sink_cs for make_cut. lca at root of sink still
           uses that version. this should be fixed *)
          let group = make_cut ~nth project lca_tid (rev_seq sink_cs) sink i in
        group ^:: acc
      | None ->
        Format.printf "Warning: no caller %d levels up from sink for \
                       callstring:\n" nth;
        print_callstring ~v:true sink_cs;
        acc)

let cuts_at_lca project src sink callgraph =
  (** Get the callstrings starting at src and sink *)
  let callstrings_srcs = callstrings project src callgraph in
  let callstrings_sinks = callstrings project sink callgraph in
  Format.printf "callstring srcs:\n";
  Output.meta "callstring srcs:\n";
  output_callstrings Output.meta callstrings_srcs;
  print_callstrings ~v:true callstrings_srcs;
  Format.printf "callstring sinks:\n";
  Output.meta "callstring sinks:\n";
  output_callstrings Output.meta callstrings_sinks;
  print_callstrings ~v:true callstrings_sinks;

  (** Get all possible combinations of src and sink *)
  let pairs = Seq.cartesian_product callstrings_srcs callstrings_sinks in

  (** lca is a sub tid *)
  let find_lca (src_cs,sink_cs) =
    let (!) = Seq.to_list_rev in
    List.filter !src_cs (fun node_src ->
        List.exists !sink_cs ~f:(fun node_sink ->
            Tid.name node_src = Tid.name node_sink)) |> List.hd in

  (** Create cut groups for each pair that share a lca *)
  Seq.fold ~init:(0,Seq.empty) pairs ~f:(fun (id,acc) (src_cs,sink_cs) ->
      match find_lca (src_cs,sink_cs) with
      | Some lca_tid ->

        let blk_tids_of_caller tid x =
          Term.enum blk_t (Util.sub_of_tid project tid |> Util.val_exn)
          |> Seq.filter ~f:(fun blk -> Util.contains_call blk x)
          |> Seq.map ~f:Util.tid_of_blk in

        let (!) = Seq.to_list_rev in

        let src_caller_sub_tid = List.nth_exn !src_cs 1 in
        let sink_caller_sub_tid = List.nth_exn !sink_cs 1 in

        (* Convert blk tids of a calling sub to the actual blks *)
        let blks_of_caller sub =
          Seq.map ~f:(fun tid -> Util.blk_of_tid sub tid) in

        let src_callstring = src_cs in
        let sink_callstring = sink_cs in
        let src_caller_sub =
          Util.sub_of_tid project src_caller_sub_tid |> Util.val_exn in
        let sink_caller_sub =
          Util.sub_of_tid project sink_caller_sub_tid |> Util.val_exn in
        let src_caller_blks =
          blk_tids_of_caller src_caller_sub_tid src
          |> blks_of_caller src_caller_sub in
        let sink_caller_blks =
          blk_tids_of_caller sink_caller_sub_tid sink
          |> blks_of_caller sink_caller_sub in
        let sub =
          Util.sub_of_tid project lca_tid |> Util.val_exn in
        let lca_name = Sub.name sub in
        let lca_sub =
          let src_str = Sub.name src_caller_sub in
          let sink_str = Sub.name sink_caller_sub in
          Sub.with_name sub (src_str^"_"^sink_str^"_"^(Sub.name sub)) in
        let (!) = Seq.length in
        (* -1 for main, -1 for actual call *)
        let depth =
          Int.max !src_callstring !sink_callstring -2 in

        let group =
          {src_caller_blks;
           sink_caller_blks;
           src_callstring;
           sink_callstring;
           src_caller_sub;
           sink_caller_sub;
           lca_name; lca_sub;
           depth; id} in

        (id+1,group ^:: acc)
      | None -> (id,acc)) |> snd

(** Operates on the callgraph. Returns meta information about all
    unique source/sink combinations *with respect to the
    callgraph*.For instance, where sources and sinks are located,
    their lowest common ancestor, and depth.*)
let cuts project callgraph src_config sink =
  match src_config with
  | {src_at_root = true; src_at_nth = _; _} ->
    Output.meta "Mode: source-at-root\n\n";
    lca_root_of_sink project sink callgraph
  | {src_at_root = _; src_at_nth = nth; _} when nth <> 0 ->
    Output.meta "Mode: nth-from-sink\n\n";
    lca_nth_of_sink project nth sink callgraph
  | {src_at_root = false; src_at_nth = _; src} ->
    Output.meta "Mode: lowest-common-ancestor\n\n";
    cuts_at_lca project src sink callgraph

let should_remap blk sink_blk_tids =
  Seq.exists sink_blk_tids ~f:(fun blk_tid ->
      Util.tid_of_blk blk = blk_tid)

let debug d src_tid sink_tid sub i =
  if d then begin
    Format.printf "Tids of src/sink based on blk structure:\n";
    Format.printf "Sub: %s %a\n" (Sub.name sub) Tid.pp (Term.tid sub);
    Format.printf "%a -> %a\n%!" Tid.pp src_tid Tid.pp sink_tid;
    Format.printf "Outputting graphs...\n";
    let dummy_path = src_tid ^:: Seq.empty in
    let open Color in
    let filename =
      Format.sprintf "pre_trim_%s_%s_%d.dot"
        (Tid.name src_tid) (Tid.name sink_tid) i in
    let special =
      [(Tid.name src_tid, !Blue); (Tid.name sink_tid, !Red)] in
    Profile.output_dot_cfg_path
      ~special
      sub dummy_path
      ~filename;
    Format.printf "Done.\n\n"
  end
