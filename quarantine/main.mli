open Core_kernel.Std
open Bap.Std
open Spec_types
open Taint

class context : program term -> int -> object('s)
    inherit Taint.context
    inherit Biri.context
    method step : 's option
    method set_restore : tid -> 's
    method pop_restore : (tid * 's) option

    method taint_var : tid -> var -> Bil.result -> 's
    method taints_of_term : tid -> taints Var.Map.t
    method taints_of_var : tid -> var -> taints
  end


val run : project -> int -> [
    | `Addr of addr
    | `Name of string
    | `Term of tid] -> context
