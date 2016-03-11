open Core_kernel.Std
open Bap.Std

exception Entry_point_not_found

class type result = object
  method visited : int Tid.Map.t
  method tainted_regs : tid -> Taint.map
  method tainted_ptrs : tid -> Taint.map
end

val run : project -> int -> [
    | `Name of string
    | `Term of tid] -> result
