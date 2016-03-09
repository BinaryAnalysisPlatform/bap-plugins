open Core_kernel.Std
open Bap.Std

class type result = object
  method visited : int Tid.Map.t
  method tainted_regs : tid -> Taint.map
  method tainted_ptrs : tid -> Taint.map
end

val run : project -> int -> [
    | `Addr of addr
    | `Name of string
    | `Term of tid] -> result
