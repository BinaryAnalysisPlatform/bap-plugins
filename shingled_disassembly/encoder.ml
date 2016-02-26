open Bap.Std
open Core_kernel.Std
open Or_error
module Lifted_image=Lifted_image_piqi


let encode mem bil accu =
  let length = Memory.length mem in
  let offset = Memory.min_addr mem |> Addr.to_int64 |> ok_exn in
  let bil_piqi = bil >>| Bil_piqi.stmts_to_piqi >>| (fun bil_piqi ->
  { Lifted_image.Lifted_instruction.instruction
    =bil_piqi; length; offset }) in
  bil_piqi :: accu
