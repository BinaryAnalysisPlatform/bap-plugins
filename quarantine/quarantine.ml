open Core_kernel.Std
open Bap.Std
open Format
open Spec
open Specification
let k = 1000

let any_of pts =
  List.map pts ~f:Re_posix.re |>
  Re.alt |>
  Re.compile |>
  Re.execp

let is_interesting = any_of [
    ".*"
  ]

let is_interesting_sub sub =
  is_interesting (Sub.name sub)

let print_trace trace =
  List.iter trace ~f:(Format.printf "%a@." Tid.pp)

type marker = {mark : 'a. 'a term -> 'a term}

let mark_if_visited ctxt =
  let vis = Tid.Set.of_list ctxt#trace in
  let mark t =
    if Set.mem vis (Term.tid t)
    then Term.set_attr t foreground `green else t in
  {mark}

let mark_if_tainted (ctxt : Main.context) =
  let mark t =
    let vars = ctxt#taints_of_term (Term.tid t) in
    if Map.is_empty (vars)
    then t else
      Term.set_attr (Term.set_attr t foreground `red)
        Taint.vars vars in
  {mark}

let if_seeded =
  let mark t =
    if Term.has_attr t Taint.seed
    then Term.set_attr t background `red else t in
  {mark}

let seed tid =
  let mark t =
    if Term.tid t = tid
    then Term.set_attr t Taint.seed tid else t in
  {mark}

let colorize c tid = { mark = fun t ->
    if Term.has_attr t Taint.seed
    then Term.set_attr t color c else t
  }

let unseed_if_nongreen = {
  mark = fun t ->
    if Term.get_attr t foreground = Some `green then t
    else Term.del_attr t Taint.seed
}


let marker_of_markers markers =
  let mark t =
    List.fold markers ~init:t ~f:(fun t {mark} -> mark t) in
  {mark}

let mark_terms {mark} prog  =
  Term.map sub_t prog ~f:(fun sub ->
      mark sub |>
      Term.map arg_t ~f:mark |>
      Term.map blk_t ~f:(fun blk ->
          mark blk |>
          Term.map phi_t ~f:mark |>
          Term.map def_t ~f:mark |>
          Term.map jmp_t ~f:mark))

let contains_seed sub =
  let is_seeded t = Term.has_attr t Taint.seed in
  let has t p = Term.enum t p |>
                Seq.exists ~f:is_seeded in
  has arg_t sub ||
  Term.enum blk_t sub |> Seq.exists ~f:(fun blk ->
      has phi_t blk || has def_t blk)

let seeded callgraph subs =
  let callers sub =
    Graphlib.fold_reachable (module Graphlib.Callgraph) callgraph
      ~rev:true ~init:Tid.Set.empty ~f:Set.add (Term.tid sub) in
  Seq.filter subs ~f:contains_seed |>
  Seq.fold ~init:Tid.Set.empty ~f:(fun subs sub ->
      Set.union subs @@ callers sub)

let main proj =
  eprintf "%a" Spec.pp spec;
  let s = Solver.create spec in
  let prog = Project.program proj |>
             Solver.seed s in
  let proj = Project.with_program proj prog in
  let callgraph = Program.to_graph prog in
  let subs = Term.enum sub_t prog |>
             Seq.filter ~f:is_interesting_sub |>
             seeded callgraph in
  let total = Set.length subs in
  let count = ref 0 in
  let proj =
    Term.enum sub_t prog |>
    Seq.filter ~f:(fun sub -> Set.mem subs (Term.tid sub)) |>
    Seq.fold ~init:proj ~f:(fun proj point ->
        incr count;
        let perc = (float !count /. float total) *. 100. |>
                   Int.of_float in
        eprintf "%-40s [%d/%d] %3d%%\r%!"
          (Sub.name point) !count total perc;
        let ctxt = Main.run proj k (`Term (Term.tid point)) in
        let mark = marker_of_markers [
            mark_if_visited ctxt;
            mark_if_tainted ctxt;
          ] in
        let prog = Project.program proj |> mark_terms mark in
        Project.with_program proj prog) in
  eprintf "\r%-80s\r%!" "Solving...";
  let prog = Project.program proj |> mark_terms unseed_if_nongreen in
  let sol = Solver.solve s prog in
  printf "%a" (Solver.pp_solution `unsatisfied) sol;
  printf "%a" (Solver.pp_solution `satisfied) sol;
  proj


let () = Project.register_pass "quarantine" main
