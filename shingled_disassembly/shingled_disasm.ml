(* TODO uses the conservative disassembler with functions that *)
   (* operate on meta data about the run *)
open Bap.Std
open Core_kernel.Std
open Mem

module Dis = Disasm_expert.Basic
let count_weight _ =
  ()

let lift ?min_addr arch string_mem =
  let addr_size= Size.to_bits @@ Arch.addr_size arch in
  let min_addr = Option.value min_addr ~default:(Addr.of_int addr_size 0) in
  let gmem = (create_memory arch string_mem min_addr) in
  Dis.create ~backend:"llvm" (Arch.to_string arch) >>= fun dis ->
  let module Target = (val target_of_arch arch) in
  return (lifted_of_bil @@ Conservative_disasm.run_with dis gmem count_weight)
