open Core_kernel.Std
open Bap.Std

let warn_opt = ref true

(** See Util.contains_call...*)
(** With @: Tid.name. Without @: Sub.name *)
let filtered_call ~(f : Filter.t) ccall =
  let open Option in
  (Util.target_tid_of_call ccall >>=
   fun tid -> Tid.name tid |> (Filter.apply f) |> some) |>
  Option.value ~default:false

let filtered_sub ~f sub =
  "@"^(Sub.name sub) |> (Filter.apply f)

(* This is a top-down cloning-based approach, followed by inlining
   cloned functions. Skips recursive functions. Does not handle
   mutual recrusive. (Dragon Book P910 and P.914) *)

(** When we clone a sub that we inline, we get one with terms that
    have new tids. But the tids in jmps and calls are not updated to
    reflect this. We want to retain the CFG structure, so map from
    the old tids to the new one. *)
(** For each jump in each block of sub: Jump is a goto -> replace
    with table Jump is a Call -> replace call and ret label Jump is
    Int -> replace label *)
let remap_tids sub tids =
  let open Option in
  let replace_tids blk = Term.map jmp_t blk ~f:(fun jmp ->
      match Jmp.kind jmp with
      | Goto (Direct tid) ->
        let new_tid = Tid.Table.find_exn tids tid in
        Jmp.create_goto ~cond:(Jmp.cond jmp) (Direct new_tid)
      | Call t ->
        (Call.return t >>= function
          | Direct tid ->
            let new_return_tid = Tid.Table.find_exn tids tid in
            let new_call =
              Call.create ~return:(Direct new_return_tid)
                ~target:(Call.target t) () in
            Some (Jmp.create_call ~cond:(Jmp.cond jmp) new_call)
          | _ -> None) |>
        Option.value ~default:jmp
      | Int (i,tid) ->
        (Tid.Table.find tids tid >>= fun new_tid ->
         Jmp.create_int ~cond:(Jmp.cond jmp) i new_tid |> some) |>
        Option.value ~default:jmp (** Use the orignal jump if not found *)
      | _ -> jmp) in
  Term.map blk_t sub ~f:replace_tids

let remap_exit_blocks blk return_tid =
  if Util.contains_indirect_goto blk then (** it's an exit block *)
    Term.append jmp_t blk @@ Jmp.create_goto (Direct return_tid)
  else if Util.ends_with_lr blk then
    (** TODO: erase the "return LR" instruction ? Do we care? *)
    Term.append jmp_t blk @@ Jmp.create_goto (Direct return_tid)
  else
    blk

let inline_sub_called_by_blk project ccall callsite_tid blk sub =
  let open Option in
  Util.target_tid_of_call ccall >>= fun target_tid ->
  Util.sub_of_tid project target_tid >>= fun target_sub ->
  Util.get_return_target ccall >>= fun return_target -> (** valid ret *)
  match Util.calls_self target_sub with
  | true ->
    if !warn_opt then
      Format.printf "Warning: Skipping sub %S: recursive\n" @@
      Sub.name target_sub;
    None
  | false ->
    let cloned_target_sub,tid_table =
      Util.deep_clone_sub target_sub in
    let fixed_sub = remap_tids cloned_target_sub tid_table in
    let entry_blk_tid =
      Term.first blk_t fixed_sub
      |> Util.val_exn
      |> Util.tid_of_blk in
    (** Update the goto jmp of parent *)
    let fixed_parent_blk =
      Term.update jmp_t blk
        (Jmp.create_goto ~tid:callsite_tid (Direct entry_blk_tid)) in
    (** update the sub with new block *)
    let updated_sub_with_blk =
      Term.update blk_t sub fixed_parent_blk in
    (** append all the blocks in fixed_sub. fixup exit blocks if needed *)
    let res =
      Term.enum blk_t fixed_sub |>
      Seq.fold ~init:(updated_sub_with_blk, [])
        ~f:(fun (sub,new_blks) blk ->
            let res = remap_exit_blocks blk return_target in
            (Term.append blk_t
               ~after:(Term.tid fixed_parent_blk) sub res),
            (Term.name blk)::new_blks)
    in Some res

(** For a blk with a call, inline the call and return an updated
    global sub. Skip recursive. *)
let inline_update_blk project sub blk_name (filtr: Filter.t) =
  let blk = Util.blk_of_tid sub @@ Tid.(!blk_name) in
  match Util.calls_of_blk_with_tid blk with
  | [] -> sub,[]
  | [(callsite_tid,ccall)] when not (filtered_call ~f:filtr ccall) ->
    inline_sub_called_by_blk project ccall callsite_tid blk sub
    |> Option.value ~default:(sub,[])
  | _ -> sub,[] (** Call is filtered *)

(** Fold over blocks containing calls. For each block, inline the
    callee. Skip recursive functions. This inlines with cloning ->
    deep clone upon encounter.*)
let inline project sub (filtr : Filter.t) =
  let tids = Util.blks_with_calls sub in
  List.fold tids ~init:(sub,[]) ~f:(fun (sub,new_blks) blk_name ->
      let res,addition =
        inline_update_blk project sub blk_name filtr in
      res,addition @ new_blks)

let make_color_map l c =
  List.fold l ~init:[] ~f:(fun acc name ->
      List.Assoc.add acc name c)

(** Inline n times. Note we don't have to fail if we detect mutually
    recursive calls. Just warn. *)
let inline_n ?(warn=true) project sub filtr n =
  warn_opt := warn;
  let callgraph_of_sub = Util.callgraph_of_sub project (Util.tid_of_sub sub) in
  if Util.is_mutually_recursive callgraph_of_sub then
    if !warn_opt then
      Format.printf
        "Warning: callgraph of sub %s contains mutually recursive calls\n"
      @@ Sub.name sub;
  let open Color in
  let l = Color.monochrome_gradient in
  let sub =  Sub.with_name sub ("clone_inlined_"^(Sub.name sub)) in
  let rec fp current prev color_map n =
    match n with
    | 0 -> current,color_map
    | _ ->
      let inlined_sub,new_blks = inline project current filtr in
      let highlight = make_color_map new_blks
          (List.nth_exn l (n % List.length l)) in
      fp inlined_sub current (color_map @ highlight) (n-1) in
  let inlined_sub,new_blks = inline project sub filtr in
  let highlight = make_color_map new_blks
      (List.nth_exn l (n % List.length l)) in
  (** n-1 because we already performed one round of inlining *)
  fp inlined_sub sub highlight (n-1)

(** Successively call inline until fp *)
let inline_fixpoint ?(warn=true) project sub (filtr : Filter.t) =
  warn_opt := warn;
  let callgraph_of_sub = Util.callgraph_of_sub project (Util.tid_of_sub sub) in
  if Util.is_mutually_recursive callgraph_of_sub then
    failwith @@
    sprintf "Callgraph of sub %s contains mutually recursive calls"
    @@ Sub.name sub;
  let open Color in
  let l = Color.monochrome_gradient in
  let sub =  Sub.with_name sub ("clone_inlined_"^(Sub.name sub)) in
  let rec fp current prev color_map i =
    (** _v "\t[*]" @@ sprintf "Iteration %d" i;*)
    let n1 = Term.enum blk_t current |> Seq.length in
    let n2 = Term.enum blk_t prev |> Seq.length in
    match n2 - n1 with
    | 0 -> current,color_map
    | _ ->
      let inlined_sub,new_blks = inline project current filtr in
      let highlight = make_color_map new_blks
          (List.nth_exn l (i % List.length l)) in
      fp inlined_sub current (color_map @ highlight) (i+1) in
  let i = 0 in
  let inlined_sub,new_blks = inline project sub filtr in
  let highlight = make_color_map new_blks
      (List.nth_exn l (i % List.length l)) in
  fp inlined_sub sub highlight i

(* Bottom-up construction supergraph construction, with summary-edge
   approach. (Dragon Book P.911 - 914). This is really just to
   illustrate how we can visually construct a compacted supergraph,
   based on summary edges (P.913). The "summary" here is just the
   function CFG. *)

(** This is a visual representation of compacted supergraphs,
    similar to what we might want for summary-based approaches. We
    do this as summary as follows: 1) bottom up, create summaries
    for sub -> [callsite lookup ->{ target, return}] 2) bottom up,
    build a new sub. add if not exists. once added, use summary to
    replace calls with gotos. *)

  (** maps a call site tid to target, return *)
type edge_in = {src: tid; dst: tid}
(** could have more than one exit block. therefore, we keep a list of
    blk tids in the former *)
type edge_out = {srcs: tid list; dst: tid}
type csite_entry = {edge_in : edge_in; edge_out : edge_out}
(** will be indexed by callsite tid, = edgeIn.src *)
type cgraph_summary =  csite_entry Tid.Table.t

let create_entry project jmp_tid ccall =
  let open Option in
  Util.target_tid_of_call ccall >>= fun target_tid ->
  Util.sub_of_tid project target_tid >>= fun target_sub ->
  Util.get_return_target ccall >>= fun return_target ->
  match Util.calls_self target_sub with
  | true ->
    (**Format.printf "Skipping sub %S: recursive\n" @@ Sub.name
       target_sub; print_newline ();*)
    None
  | false ->
    (** target_tid will point to the sub label, like @bar.
        That doesn't work when we construct from blocks, so we
        need the tid of the first block *)
    let target_tid =
      Util.sub_of_tid project target_tid
      |> Util.val_exn
      |> Term.first blk_t
      |> Util.val_exn
      |> Util.tid_of_blk in

    let edge_in:edge_in = {src = jmp_tid; dst = target_tid} in
    (** every exit block of the 'inlined' sub has to be
        recorded here *)
    let exit_blk_tids =
      Term.enum blk_t target_sub
      |> Seq.to_list
      |> List.filter ~f:(fun blk -> Util.is_exit_block target_sub blk)
      |> List.map ~f:Term.tid in

    let edge_out:edge_out = {srcs = exit_blk_tids;
                             dst = return_target} in
    Some {edge_in; edge_out}

let make_summary project sub filtr =
  Term.enum blk_t sub |>
  let table = Tid.Table.create () in

  Seq.fold ~init:table ~f:(fun table blk ->
      match Util.calls_of_blk_with_tid blk with
      | [] -> table (** no calls *)
      | [(jmp_tid,ccall)] when not (filtered_call ~f:filtr ccall) ->
        begin
          match create_entry project jmp_tid ccall with
          | Some entry ->
            Tid.Table.add_exn table ~key:jmp_tid ~data:entry; table
          | None -> table
        end
      | _ -> table)

(** add blocks, but not if they are filtered *)
let add_blocks new_sub sub filtr =
  if filtered_sub ~f:filtr sub then
    new_sub
  else
    Seq.fold ~init:new_sub (Term.enum blk_t sub) ~f:(fun new_sub blk ->
        Term.append blk_t new_sub blk)

(** key and src_in are the same *)
let replace_calls_with_gotos key src_in dst_in new_sub =
  Term.map blk_t new_sub ~f:(fun blk ->
      let callsite = Term.find jmp_t blk key in
      match callsite with
      | Some callsite ->
        Term.update jmp_t blk (Jmp.create_goto ~tid:src_in (Direct dst_in))
      | None -> blk)

let remap_exit_blks exit_blocks dst_out new_sub =
  Term.map blk_t new_sub ~f:(fun blk ->
      let blk_tid = Util.tid_of_blk blk in
      if List.exists exit_blocks ~f:(fun b ->
          (Tid.name b) = (Tid.name blk_tid)) then
        Term.append jmp_t blk @@ Jmp.create_goto (Direct dst_out)
      else
        blk)

(** Note: using Builder would be nice in order to construct bottom
    up and add subs as we encounter them. The problem is when we
    want to update exit blocks of leaf subs that are already added.
    There's no way to do that with Builder, unless we make the sub,
    do our relabelling, and start building again. For this reason,
    it's easier to resort to 'Term.append' on the empty sub *)
let construct_bottom_up project table sub_name sub_callgraph filtr =
  (** In post order *)
  let subs_to_add =
    Graphlib.postorder_traverse (module Graphlib.Callgraph) sub_callgraph |>
    Seq.filter_map ~f:(fun x -> Util.sub_of_tid project x) in
  let exists = Sub.Set.empty in (** whether Tid or Sub? *)
  let new_sub = Sub.Builder.create ~name:("new_compacted_"^sub_name) () |>
                Sub.Builder.result in
  Seq.fold ~init:(new_sub,exists) subs_to_add ~f:(fun (new_sub,exists) sub ->
      if (Sub.Set.mem exists sub) then
        sub,exists
      else
        let this_sub_summary = Sub.Table.find_exn table sub in
        (** Add each block. For each block, update with a goto for
            this subs callsites. Destination blocks should already be
            present, because this is bottom-up!*)
        let new_sub = add_blocks new_sub sub filtr in
        let res = (** update summary edges *)
          Tid.Table.fold ~init:new_sub this_sub_summary
            ~f:(fun ~key ~data new_sub ->
                let src_in = data.edge_in.src in
                let dst_in = data.edge_in.dst in
                let exit_blocks = data.edge_out.srcs in
                let dst_out = data.edge_out.dst in
                new_sub
                |> replace_calls_with_gotos key src_in dst_in
                |> remap_exit_blks exit_blocks dst_out)
        in res,(Sub.Set.add exists sub))

let debug_table =
  Sub.Table.iter ~f:(fun ~key ~data ->
      Format.printf "K:%s\n" @@ Sub.name key;
      Tid.Table.iter data ~f:(fun ~key ~data ->
          Format.printf
            "\tCallsite: %s\n\t\t1: %s\n\t\t2:%s\n\t\t3:%s\n\t\t4:%s\n"
            (Tid.name key)
            (Tid.name data.edge_in.src)
            (Tid.name data.edge_in.dst)
            (List.to_string data.edge_out.srcs ~f:Tid.name)
            (Tid.name data.edge_out.dst)))

(** Create a compacted supergraph rooted at sub. For each sub called
    in the callgraph of sub (by transitive closure), add the sub's
    blocks, and introduce summary edges to the entry and exit blocks
    of the add sub. *)
let compacted_summary project sub filtr =
  let open Option in
  let open Summary in
  let sub_callgraph = Program.to_graph
    @@ Util.callgraph_of_sub project @@ Util.tid_of_sub sub in
  let f node : 'a Summary.t option =
    let module G = Graphlib.Callgraph.Node in
    Util.sub_of_tid project (G.label node) >>= fun subroutine ->
    let data = make_summary project subroutine filtr in
    let summary = {subroutine; data} in
    Some summary in
  let table = Summary.summarize_bottom_up sub_callgraph ~f in
  (**debug_table table;*)
  let big_sub,_ = construct_bottom_up project table
      (Sub.name sub) sub_callgraph filtr in
  big_sub
