open Core_kernel.Std
open Bap.Std

type t = int
val null   : t
val create : string -> t
include Regular with type t := t
