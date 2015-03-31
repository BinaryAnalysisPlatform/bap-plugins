open Core_kernel.Std
open Bap.Std
open Program_visitor
open Format

let run color = register' (fun p ->
    Memmap.to_sequence p.annots |>
    Seq.iter ~f:(fun (mem,(name,value)) ->
        if name = "staticstore" then match color,value with
          | `all,_ | `green, "green" | `red, "red" ->
            Option.iter (Table.find p.symbols mem) ~f:(fun sym ->
                if color = `all
                then printf "%s %s@." sym (String.uppercase value)
                else printf "%s@." sym)
          | _ -> ()))
