open Core_kernel.Std
open Bap.Std
open Format

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
    then Term.set_attr t foreground `green
    else t in
  {mark}

let mark_if_tainted (ctxt : Main.context) =
  let mark t =
    if Map.is_empty (ctxt#taints_of_term (Term.tid t))
    then t else Term.set_attr t foreground `red in
  {mark}

let mark_if_seeded =
  let mark t =
    if Term.has_attr t Taint.seed
    then Term.set_attr t foreground `black else t in
  {mark}

let seed tid =
  let mark t =
    if Term.tid t = tid
    then Term.set_attr t Taint.seed tid else t in
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
  let prog = Project.program proj in
  let src1 = seed (Tid.from_string_exn "%86") in
  let prog = mark_terms src1 prog in
  let ctxt = Main.run prog k point in
  let if_visited = mark_if_visited ctxt in
  let if_tainted = mark_if_tainted ctxt in
  let if_seeded = mark_if_tainted ctxt in
  let prog = mark_terms if_visited prog in
  let prog = mark_terms if_tainted prog in
  let prog = mark_terms if_seeded prog in
  Format.printf "@.";
  Project.with_program proj prog

let () = Project.register_pass "quarantine" main
