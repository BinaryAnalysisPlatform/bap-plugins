open Core_kernel.Std
open Bap.Std

type var = [
  | `Reg of Var.t
  | `Pos of int
  | `Ret
  | `All
] with bin_io, compare, sexp


type exp = [
  | `Var of var
  | `Mem of var * int
] with bin_io, compare, sexp


type arg = exp * word option
with bin_io, compare, sexp

type args = arg list
with bin_io, compare, sexp

type ident = [
  | `Addr of addr
  | `Name of string
  | `Term of tid
] with bin_io, compare, sexp

type call = ident * args
with bin_io, compare, sexp

type cure = [
  | `Any of cure * cure
  | `Seq of cure * cure
  | `Call of call
] with bin_io, compare, sexp


type host = [
  | `Call of call
  | `Read of exp
  | `Seq of host * host
] with bin_io, compare, sexp

type spec = (host * cure option) list
with bin_io, compare, sexp
