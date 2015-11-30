open Bap.Std

(** Priority of analysis result. Should probably not be in policy module *)
type priority

module Predicate : sig
  (** Provides a set of predicates that can be used in an analysis over paths *)

  (** Whether an argument is a constant (0) for a given def in a blk *)
  val arg_is_const_0 :  Blk.t -> tid -> bool

  (** Argument is some const *)
  val arg_is_const : Blk.t -> tid -> bool

  (** Argument is not a const (symbolic) *)
  val arg_is_not_const : Blk.t -> tid -> bool

  (** Deprecated: use _string_ version. Argument is a memory value
      that points to rodata *)
  val arg_is_rodata : project -> Blk.t -> tid -> bool

  (** Deprecated: use _string_ version. Argument is not a memory value
      that points to rodata *)
  val arg_is_not_rodata : project -> Blk.t -> tid -> bool

  (** Deprecated: use _string_ version. Argument is a memory value
      that points to rodata, and contains string *)
  val arg_rodata_contains : project -> blk term -> tid -> string -> bool

  val arg_is_string : project -> Blk.t -> tid -> bool

  val arg_is_not_string : project -> Blk.t -> tid -> bool

  val arg_string_contains : project -> blk term -> tid -> string -> bool

  (** The path (subroutine) contains a call, e.g., "@.malloc" *)
  val contains_call : string -> Sub.t -> bool

  (** The path (subroutine) contains any of these call, e.g., "@.malloc" *)
  val contains_calls : string list -> Sub.t -> bool

  (** Check whether defs containt an ITE (dependencies) *)
  val contains_ite : Sub.t -> tid seq -> bool

  val size_gt : 'a seq -> int -> bool
  val size_lt : 'a seq -> int -> bool
  val size_eq : 'a seq -> int -> bool
  val size_eq_0 : 'a seq -> bool
  val size_eq_1 : 'a seq -> bool
end

val dep_blk_span : tid seq -> Sub.t -> int

val get_arg_as_string : project -> blk term -> tid -> string option

val get_arg_as_const : project -> Blk.t -> Tid.t -> int option
