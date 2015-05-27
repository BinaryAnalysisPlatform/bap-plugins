open Core_kernel.Std
open Bap.Std
open Project

let code_of_color = function
  | `green -> 0x99ff99
  | `red -> 0xCCCCFF
  | `yellow -> 0xC2FFFF

let () = register_plugin (fun p -> {
      p with
      memory = Memmap.map p.memory ~f:(fun tag ->
          match Value.get color tag with
          | None -> tag
          | Some color -> match color with
            | `red | `green | `yellow as c ->
              sprintf
                "SetFunctionAttr($symbol_addr, FUNCATTR_COLOR, 0x%x)\n"
                (code_of_color c) |>
              Value.create python
            | _ -> tag)
    })
