open Core_kernel.Std
open Bap.Std
open Spec_types

type t

type input

val create : spec -> t

val mark_inputs : t -> program term -> program term

val input : input tag

module Input : sig
  type t = input

  val definition : t -> definition
  val judgement : t -> judgement
  val var : t -> v
  include Regular with type t := t
end
