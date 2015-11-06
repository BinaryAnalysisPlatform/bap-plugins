open Core_kernel.Std
open Bap.Std
open Format
open Spec
open Specification
let k = 100
let point = `Name "main"

let pp_taints c ppf taints =
  Tid.Set.iter taints ~f:(fprintf ppf "%c:%a@." c Tid.pp)

let print_result (`Cured c, `Uncured u, `Dead d) =
  printf "%a" (pp_taints 'c') c;
  printf "%a" (pp_taints 'u') u;
  printf "%a" (pp_taints 'd') d

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
  let mark t = t in
  (* if Term.has_attr t Taint.seed *)
  (* then Term.set_attr t background `red else t in *)
  {mark}

let seed tid =
  let mark t =
    if Term.tid t = tid
    then Term.set_attr t Taint.seed tid else t in
  {mark}



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

let main proj =
  eprintf "%a" Spec.pp spec;
  let s = Solver.create spec in
  let proj =
    Project.program proj |>
    Solver.seed s |>
    Project.with_program proj in
  let ctxt = Main.run proj k point in
  let mark = marker_of_markers [
      mark_if_visited ctxt;
      mark_if_tainted ctxt;
      if_seeded;
    ] in
  let prog = Project.program proj |>
             mark_terms mark in
  let sol = Solver.solve s prog in
  printf "%a" (Solver.pp_solution `satisfied) sol;
  Project.with_program proj prog

let () = Project.register_pass "quarantine" main
