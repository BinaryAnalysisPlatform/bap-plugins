(* Uses conservative disasm to get semantics for every valid instruction *)
open Bap.Std
open Core_kernel.Std
open Or_error
open Mem
module Dis = Disasm_expert.Basic

module type InsnLifter = sig
  val lift : Memory.t -> Dis.full_insn -> bil t
end

module type BilEncoder = sig
  type result
  val encode : Memory.t -> stmt list t ->
    result t list -> result t list
end

module Lifted_image=Lifted_image_piqi

module Image_lifter(Lifter : InsnLifter) (Encoder : BilEncoder) =
struct
  type result = Encoder.result
  let lift_image ?min_addr arch string_mem =
    let addr_size= Size.to_bits @@ Arch.addr_size arch in
    let min_addr = Option.value min_addr ~default:(Addr.of_int addr_size 0) in
    let gmem = (create_memory arch string_mem min_addr) in
    let dis = Dis.create ~backend:"llvm" (Arch.to_string arch) |> ok_exn in
    Conservative_disasm.run_with
      dis gmem ~at:(fun (mem,insn) accu -> match insn with
          | Some insn -> (Encoder.encode mem (Lifter.lift mem insn) accu)
          | None -> accu)
end 
