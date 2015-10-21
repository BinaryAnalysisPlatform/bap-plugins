open Core_kernel.Std
open Bap.Std
open Bil.Result
open Taint


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


class ['a] t : object
  constraint 'a = #context
  inherit ['a] expi
end
