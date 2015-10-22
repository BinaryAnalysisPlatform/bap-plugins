open Core_kernel.Std
open Bap.Std
open Spec_types
open Format

let k = 100
let point : ident = `Name "main"
let spec : spec = [`Call (`Name "malloc",[`Var (`Reg ARM.CPU.r0),None]),None]

let pp_taints c ppf taints =
  Taint.Set.iter taints ~f:(fprintf ppf "%c:%a@." c Taint.pp)

let print_result (`Cured c, `Uncured u, `Dead d) =
  printf "%a" (pp_taints 'c') c;
  printf "%a" (pp_taints 'u') u;
  printf "%a" (pp_taints 'd') d

let print_trace trace =
  List.iter trace ~f:(Format.printf "%a@." Tid.pp)

let main proj =
  let prog = Project.program proj in
  let ctxt = Main.run prog k spec point in
  let r = Taint.compute_result ctxt in
  print_trace (List.rev ctxt#trace);
  print_result r


let () = Project.register_pass' "quarantine" main
