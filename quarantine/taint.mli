open Core_kernel.Std
open Bap.Std
open Bil.Result
open Spec_types


include Regular
type taints = Set.t

val create : host -> tid -> t

val host : t -> host

val term : t -> tid

val pp_taints : Format.formatter -> taints -> unit

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

val compute_result : #context ->
  [`Cured of taints] *
  [`Uncured of taints] *
  [`Dead of taints]
