open Core_kernel.Std
open Regular.Std
open Bap.Std

type t = int
val null   : t
val create : string -> t
include Regular.S with type t := t
