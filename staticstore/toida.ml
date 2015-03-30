open Core_kernel.Std
open Bap.Std
open Program_visitor


let green = 0x99ff99
let red   = 0xCCCCFF
let yellow  = 0xC2FFFF

let emit_insn color =
  sprintf "SetFunctionAttr($min_addr, FUNCATTR_COLOR, 0x%x)\n" @@
  match color with
  | "red" -> red
  | "green" -> green
  | "yellow" -> yellow
  | s -> invalid_arg s

let () = register (fun p -> {
      p with
      annots = Memmap.map p.annots ~f:(fun (tag,value) ->
          if tag = "staticstore" then "idapy", emit_insn value
          else (tag,value))
    })
