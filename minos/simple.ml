open Bap.Std
open Core_kernel.Std
open Options
open Ctxt

let simplify ctxt sub_path =
  let arch = Project.arch ctxt.project in
  let mem_to_reg = Mem_to_reg.analyze in
  let fold_consts =
    Fold_consts.analyze ~fixsp:false arch in
  let o = ctxt.options in
  match (o.mem_to_reg, o.fold_consts) with
  | true,true ->
    let res = sub_path in
    let res2 = mem_to_reg res in
    let res3 = fold_consts res2 in
    res3
  | true,false -> sub_path |> mem_to_reg |> Sub.ssa
  | false,true -> sub_path |> fold_consts (* fold_consts ssa's by default *)
  | _,_ -> sub_path |> Sub.ssa
