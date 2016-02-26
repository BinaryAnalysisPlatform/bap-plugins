open Bap.Std
open Core_kernel.Std
module Dis = Disasm_expert.Basic
exception Create_mem of Error.t

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error e -> raise (Create_mem e)  
