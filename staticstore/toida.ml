open Core_kernel.Std
open Bap.Std

let code_of_color = function
  | `green -> 0x99ff99
  | `red -> 0xCCCCFF
  | `yellow -> 0xC2FFFF
  | _ -> invalid_arg "unexpected color"

let cmd c =
  sprintf "SetFunctionAttr($symbol_addr, FUNCATTR_COLOR, 0x%x)\n"
    (code_of_color c)

let () = Project.register_pass ~deps:["staticstore"] (fun p ->
    Project.memory p |> Memmap.to_sequence |>
    Seq.fold ~init:p ~f:(fun p (mem,v) ->
        Option.value_map ~default:p (Value.get color v)
          ~f:(fun c -> Project.substitute p mem python (cmd c))))
