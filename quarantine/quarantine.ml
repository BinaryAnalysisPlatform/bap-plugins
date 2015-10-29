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

let main proj =
  let prog = Project.program proj in
  let ctxt = Main.run prog k point in
  print_trace (List.rev ctxt#trace)


let () = Project.register_pass' "quarantine" main
