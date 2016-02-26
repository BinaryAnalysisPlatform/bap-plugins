open Core_kernel.Std
open Bap.Std

module Lifted_image = Lifted_image_piqi
open Lifted_image

module InsnLifter = struct let lift = Lift.X32.lift end

module PbEncoder : (Image_lifter.BilEncoder
  with type result = Lifted_instruction.t) = struct 
  let encode = Encoder.encode
  type result = Lifted_instruction.t
end

module ImageLifter = Image_lifter.Image_lifter(InsnLifter)(PbEncoder)

let gen_string length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let s = String.create length in
    for i=0 to (length-1) do
      String.set s i (char_of_int @@ gen())
    done;
    s

let get_size obj =
  Objsize.show_info @@ Objsize.objsize obj

let show_sizes memory bigs_memory lifted_insns obuf_lifted_insns str_encoded_lifted_insns =
  print_endline @@ "Size of string in bytes: " ^ get_size memory;
  print_endline @@ "Size of bigstring in bytes: " ^ get_size bigs_memory;
  print_endline @@ "Size of lifted memory: " ^  get_size lifted_insns;
  print_endline @@ "Size of OBuf (piqi) object of lifted memory: " ^ get_size obuf_lifted_insns;
  print_endline @@ "Size of piqi string object of " ^ get_size str_encoded_lifted_insns

let create_types memory =
  print_endline "Disassembling...";
  let lifted_insns = ImageLifter.lift_image Arch.(`x86_64) memory in
  print_endline "Obuf of lifted_region"
  (* let lifted_insns = List.filter_map lifted_insns ~f:(function
      | Ok piqi_lifted -> Some piqi_lifted
      | Error err -> None) in
  let obuf_lifted_insns = gen_lifted_region lifted_insns in
  print_endline "Piqirun.to_string";
  let str_encoded_lifted_insns = Piqirun.to_string
  obuf_lifted_insns *)

let execute_test_on_memory memory () =
  print_endline ("Creating types of string size: " ^ (string_of_int @@ String.length memory));
  create_types memory;
  print_endline "\nCollecting memory...";
  Gc.full_major ()

let () =
  print_endline "generating random strings...";
  for power=0 to 21 do (
    let size = ((Int.pow 2 power)*100) in
    print_endline @@ "Creating random string of size: " ^ string_of_int size;
    execute_test_on_memory (gen_string size) ();
  );
  done
