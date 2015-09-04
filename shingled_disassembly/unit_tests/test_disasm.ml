open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

module Dis = Disasm_expert.Basic
module Lifted_image = Lifted_image_piqi
open Lifted_image 

module InsnLifter = struct let lift = Lift.X32.lift end

module PbEncoder : (Image_lifter.BilEncoder
  with type result = Lifted_instruction.t) = struct 
  let encode = Encoder.encode
  type result = Lifted_instruction.t
end

module ImageLifter = Image_lifter.Image_lifter(InsnLifter)(PbEncoder)

exception Lift_failure of string

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the bap utility *)
let test_hits_every_byte test_ctxt =
  let arch = Arch.(`x86) in
  let addr_size= Size.to_bits @@ Arch.addr_size arch in
  let min_addr = Addr.of_int addr_size 0 in
  let memory = "\x2d\xdd\xc3\x54\x55" in
  let lifted_encoded_region = ImageLifter.lift_image ~min_addr arch memory in
  let sizes = List.filter_map lifted_encoded_region
      ~f:(fun lifted_insn ->
          match lifted_insn with
          | Ok encoded_insn ->
            let {Lifted_instruction.instruction; length; offset} =
              encoded_insn in Some length
          | Error _ -> None) in
  let expected_results = [ 5; 1; 1; 1] in
  print_endline @@ List.to_string sizes ~f:string_of_int;
  List.iter2_exn sizes expected_results
    ~f:(fun actual_size expected_size ->
      assert_equal ~msg:((List.to_string ~f:string_of_int sizes)
                           ^ (List.to_string ~f:string_of_int
                                expected_results)) actual_size
        expected_size)

let () =
  let suite = 
  "suite">:::
    [
      "test_hits_every_byte">:: test_hits_every_byte;
    ] in
  run_test_tt_main suite
;;
