open Bap.Std
open Core_kernel.Std
open Or_error
module Dis = Disasm_expert.Basic

let prev_chunk mem ~addr =
  let prev_addr = Addr.pred addr in
  Memory.view ~from:prev_addr mem

let disasm dis mem ~at =
  let rec back cur_mem accu =
    let insn = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, i, status) -> m, i
      | Error err -> cur_mem, None
    in match prev_chunk mem ~addr:(Memory.min_addr cur_mem) with
    | Ok mem ->  back mem (at insn accu) 
    | Error _ -> at insn accu in
  match (Memory.view mem ~from:(Memory.max_addr mem)) with
  | Ok mem -> (back mem [])
  | Error err -> raise (Error.to_exn err)

let run' dis mem ~at =
  disasm dis mem ~at

let run dis mem =
  run' dis mem ~at:(fun x accu -> x :: accu)

let run_with
    dis mem ~at =
  run' dis mem ~at
