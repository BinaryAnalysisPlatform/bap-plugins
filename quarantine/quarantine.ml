open Core_kernel.Std
open Bap.Std
open Spec_types
open Format

let k = 100
let point : ident = `Name "main"
let spec : spec = [`Call (`Name "malloc",[`Var (`Reg ARM.CPU.r0),None]),None]

let pp_taints ppf taints =
  Taint.Set.iter taints ~f:(fprintf ppf "%a@." Taint.pp)

let print_result (`Cured c, `Uncured u, `Dead d) =
  printf "Cured:@.%a" pp_taints c;
  printf "Uncured:@.%a" pp_taints u;
  printf "Dead:@.%a" pp_taints d

let main proj =
  let prog = Project.program proj in
  Main.run prog k spec point |>
  print_result


let () = Project.register_pass' "quarantine" main
