open Core_kernel
open Bap.Std

let mem_elt_size arch =
  let module Target = (val target_of_arch arch) in
  match Var.typ Target.CPU.mem with
  | Type.Imm _ -> assert false
  | Type.Mem (_,s) -> s
