open Core_kernel.Std
open Bap.Std
open Spec_types
open Taint

class type result = object
  method visited : Tid.Set.t
  method tainted_regs : tid -> taints Var.Map.t
  method tainted_ptrs : tid -> taints Var.Map.t
end



val run : project -> int -> [
    | `Addr of addr
    | `Name of string
    | `Term of tid] -> result
