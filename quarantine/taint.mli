open Core_kernel.Std
open Bap.Std
open Bil.Result


type t
type taint = t
include Regular with type t := t

type taints

class context :  object('s)
  inherit Expi.context
  method taint_val : Bil.result -> taints -> 's
  method taint_mem : addr -> size -> taints -> 's
  method val_taints : Bil.result -> taints
  method mem_taints : addr -> taints
  method sanitize : Bil.result -> 's

  method sanitized   : taints
  method all_taints  : taints
  method live_taints : taints
end


class ['a] propagator : object
  constraint 'a = #context
  inherit ['a] expi
end


class when_jmp_context : program term -> object
    inherit context
    inherit Biri.context
  end

class ['a] when_jmp_sanitizer : object
  constraint 'a = #when_jmp_context
  inherit ['a] biri
end
