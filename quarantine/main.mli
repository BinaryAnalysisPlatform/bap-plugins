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
  end


val run : program term -> int -> spec -> ident -> context
