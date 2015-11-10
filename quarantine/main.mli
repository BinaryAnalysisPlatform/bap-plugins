open Core_kernel.Std
open Bap.Std
open Spec_types
open Taint

class type result = object
  method visited : Tid.Set.t
  method taints_of_term : tid -> taints Var.Map.t
end



val run : project -> int -> [
    | `Addr of addr
    | `Name of string
    | `Term of tid] -> result
