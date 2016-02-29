open Core_kernel.Std
open Bap.Std
open Format

let main project =
  Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
      printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem)

let () = Project.register_pass' main
