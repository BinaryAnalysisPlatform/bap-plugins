open Core_kernel.Std
open Bap.Std
open Spec_types


class ['a] call : (ident * args) list -> object
    constraint 'a = #Taint.context
    constraint 'a = #Biri.context
    inherit ['a] biri
  end
