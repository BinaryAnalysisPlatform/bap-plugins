open Core_kernel
open Bap.Std
open Bap_plugins.Std

let usage () =
  eprintf "Performs linear sweep disassembly and lifting of raw bytes\n";
  eprintf "Usage: ./bench [<arch>] <file>\n";
  exit 1

module Dis = Disasm_expert.Basic

module Stats = struct
  let start = ref 0.
  let finish = ref 0.
  let fails = ref 0
  let insns = ref 0
  let code = ref 0
  let data = ref 0
  let update stat data = stat := !stat + data


  let print arch =
    let total = !finish -. !start in
    let total_bytes = !code + !data in
    let total_insns = !insns + !fails in
    let latency = (total /. float total_insns) *. 1e6 in
    let speed p = (float p /. total) in
    let ratio m n = (float m /. float n) *. 100. in
    printf "Statistics for the %a lifter\n" Arch.ppo arch;
    printf "Total time: %g s\n" total;
    printf "Total throughtput: %.0f kB/s\n" @@ speed (total_bytes / 1024) ;
    printf "Insn throughtput: %.0f I/s\n" @@ speed total_insns;
    printf "Insn latency: %.2f mks/I\n" latency;
    printf "Bytes processed: %d\n" total_bytes;
    printf "Data bytes: %d\n" !data;
    printf "Code bytes: %d\n" !code;
    printf "Code density: %.2f%%\n" @@ ratio !code total_bytes;
    printf "Total number of instructions: %d\n" total_insns;
    printf "Lifted instructions: %d\n" !insns;
    printf "Lifting coverage: %.2f%%\n" @@ ratio !insns total_insns
end

let disasm arch mem =
  let module Target = (val target_of_arch arch) in
  Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
      Stats.start := Unix.gettimeofday ();
      Result.return @@ Dis.run dis mem ~init:() ~return:ident
        ~stop_on:[`Valid]
        ~stopped:(fun s () ->
            Stats.finish := Unix.gettimeofday ())
        ~invalid:(fun s mem () ->
            Stats.update Stats.data (Memory.length mem);
            Dis.step s ())
        ~hit:(fun s mem insn () ->
            Stats.update Stats.code (Memory.length mem);
            match Target.lift mem insn with
            | Ok _ ->
              incr Stats.insns;
              Dis.step s ();
            | Error _ ->
              incr Stats.fails;
              Dis.step s ()))


let main arch file =
  let size = Arch.addr_size arch in
  let base = Word.zero (Size.in_bits size) in
  match Memory.of_file (Arch.endian arch) base file with
  | Error err ->
    eprintf "Error: file is not readable or regular - %s\n"
      (Error.to_string_hum err);
    exit 1
  | Ok mem -> match disasm arch mem with
    | Error err ->
      eprintf "Error: failed to initialize the disassembler - %s\n"
        (Error.to_string_hum err)
    | Ok () ->
      Stats.print arch

let read_arch s = match Arch.of_string s with
  | Some a -> a
  | None ->
    eprintf "Error: unknown architecture %s\n" s;
    eprintf "The list of known architectures:\n";
    List.iter Arch.all ~f:(eprintf "\t%a\n" Arch.ppo);
    exit 1

let read_file s =
  if Sys.file_exists s && not (Sys.is_directory s)
  then s
  else begin
    eprintf "Error: `%s' is not a regular file\n" s;
    usage ()
  end


let () =
  Plugins.run ~exclude:["bil"] ();
  match Array.length Sys.argv with
  | 2 -> main `x86_64 (read_file Sys.argv.(1))
  | 3 -> main (read_arch Sys.argv.(1)) (read_file Sys.argv.(2))
  | _ -> usage ()
