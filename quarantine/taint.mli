open Core_kernel.Std
open Bap.Std
open Bil.Result
open Spec_types


include Regular
type taints = Set.t

val create : host -> tid -> t

val host : t -> host

val term : t -> tid
