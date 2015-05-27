open Core_kernel.Std
open Bap.Std
open Project

let custom_color c =
  match c with
  | `yellow -> 0xccffff
  | `blue -> 0xffffd0

let ida_stmt_highlight c =
  sprintf "idaapi.set_item_color(DecodeInstruction($addr).ea, 0x%x)\n"
    (custom_color c)

let output_script proj =
  let memory = Memmap.map proj.memory ~f:(fun tag ->
      match Value.get color tag with
      | None -> tag
      | Some color ->
        match color with
        | `yellow | `blue as c ->
          Value.create python @@ ida_stmt_highlight c
        | _ -> tag) in
  {proj with memory}

let () = register_plugin output_script
