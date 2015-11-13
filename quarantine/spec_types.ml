open Core_kernel.Std
open Bap.Std

type id = string
with bin_io, compare, sexp

type v = V.t with bin_io, compare, sexp

module Constr = struct
  type t =
    | Dep of v * v
    | Var of v * var
    | Fun of id * v
  with bin_io, compare, sexp, variants
end

type constr = Constr.t
with bin_io, compare, sexp

module S = struct
  type t =
    | Reg
    | Ptr
  with bin_io, compare, sexp, variants
end

type s = S.t
with bin_io, compare, sexp

module Pat = struct
  type t =
    | Call of id * v * v list
    | Jump of [`call | `goto | `ret | `exn | `jmp] * v * v
    | Move of v * v
    | Load of v * v
    | Wild of v
    | Store of v * v
  with bin_io, compare, sexp, variants
end

type pat = Pat.t
with bin_io, compare, sexp
