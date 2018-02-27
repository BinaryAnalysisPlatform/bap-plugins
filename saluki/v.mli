open Core_kernel.Std
open Regular.Std
open Bap.Std

type t = int
val null   : t
val create : string -> t
module Assoc : sig
  val find : (t * 'a) list -> t -> 'a option
  val mem : (t * 'a) list -> t -> bool
end
include Regular.S with type t := t
