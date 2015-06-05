open Core_kernel.Std
open Bap.Std
open Format

let main args project =
  Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
    printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem);
  project

let () = Project.register_pass_with_args "mem_printer" main
