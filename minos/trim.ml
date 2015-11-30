open Bap.Std
open Core_kernel.Std
open Cut
open Options

type trim = {
  trim_sub : Sub.t;
  cut_group : cut_group;
  src_tid : tid;
  sink_tid : tid
}

(** mark the sink_blk with a no_return, otherwise we can't call
    Sub.ssa on the blk. Failure can pass silently here, so it's
    not ideal. We should check that at least one blk was labeled
    with no_return *)
let mark_with_no_return sub sink_blk_tid =
  Term.map blk_t sub ~f:(fun blk ->
      if Util.tid_of_blk blk = sink_blk_tid then
        Term.map jmp_t blk ~f:(fun jmp ->
            match Jmp.kind jmp with
            | Call c ->
              Jmp.create_call ~tid:(Term.tid jmp)
                ~cond:(Jmp.cond jmp)
                (Call.with_noreturn c)
            | _ -> jmp)
      else blk)

(** test for structural equality: string of right hand side of
    defs are all the same. This needs serious rethinking. *)
let cmp blk' blk =
  let def_str b =
    Term.enum def_t b |> Seq.fold ~init:"" ~f:(fun acc def ->
        let s = Exp.pps () (Def.rhs def) in
        acc^s) in
  def_str blk' = def_str blk

let tid_from_blk_structure blk sub =
  let matches =
    Term.enum blk_t sub |> Seq.filter ~f:(fun blk' ->
        (** find the blk' that matches blk, and get its tid *)
        if cmp blk' blk then true else false) in
  match Seq.length matches with
  | 0 ->
    Format.printf " Warning: No blk structure match!";
    (1,Util.tid_of_blk blk)
  | 1 -> let blk = Seq.hd_exn matches in
    (0,Util.tid_of_blk blk)
  (* This happens when there are two or more blocks in *the same*
     subroutine that call the sink and are the same structurally.
     Weird, but can happen. Unhandled case for now. *)
  | _ -> Format.printf "Warning: More than one blk match!";
    (2,Seq.hd_exn matches |> Util.tid_of_blk)

let trim sub src sink =
  let module Cfg = Graphlib.Tid.Tid in
  let cfg = Sub.to_graph sub in
  let reachable_src =
    Graphlib.fold_reachable (module Cfg)
      ~init:Cfg.Node.Set.empty ~f:Set.add
      cfg src in
  let reachable_sink =
    Graphlib.fold_reachable (module Cfg)
      ~rev:true ~init:Cfg.Node.Set.empty ~f:Set.add
      cfg sink in
  let cut_set =
    Set.inter reachable_src reachable_sink in
  Term.filter blk_t sub ~f:(fun blk ->
      Set.mem cut_set (Term.tid blk))

(* returns a sequence of subs that start at the source and go to
   the sink. sub is lca in this case *)
let trims sub g highlight with_dots =
  let blks_call_src = g.src_caller_blks in
  let blks_call_sink = g.sink_caller_blks in
  let pairs = Seq.cartesian_product blks_call_src blks_call_sink in
  Seq.foldi ~init:Seq.empty pairs ~f:(fun j acc (src_blk, sink_blk) ->
      let res_src,src_tid = tid_from_blk_structure src_blk sub in
      let res_sink,sink_tid = tid_from_blk_structure sink_blk sub in
      if res_src = 0 && res_sink = 0 then (* If the source and sink exist *)
        ( let sub' = trim sub src_tid sink_tid in
          match Term.length blk_t sub' with
          | 0 ->
            if with_dots then
              Output.cut_graph `Invalid src_tid sink_tid sub g.id j;
            Format.printf "Sink is before source!\n%!"; acc
          | _ ->
            if with_dots then
              Output.cut_graph `Valid src_tid sink_tid sub g.id j;
            let sub' = mark_with_no_return sub' sink_tid in
            let t = {trim_sub = sub';
                     src_tid;
                     sink_tid;
                     cut_group = g} in
            t ^:: acc)
      else
        (if with_dots then
           Output.cut_graph `Skipped src_tid sink_tid sub g.id j;
         Format.printf "[x] Skipping pair %d->%d\n" res_src res_sink;
         acc))
