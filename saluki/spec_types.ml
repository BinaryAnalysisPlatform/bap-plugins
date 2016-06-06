open Core_kernel.Std
open Bap.Std

type id = string
  [@@deriving bin_io, compare, sexp]

type v = V.t [@@deriving bin_io, compare, sexp]

module Constr = struct
  type t =
    | Dep of v * v
    | Var of v * var
    | Fun of id * v
    [@@deriving bin_io, compare, sexp, variants]
end

type constr = Constr.t
  [@@deriving bin_io, compare, sexp]

module S = struct
  type t =
    | Reg
    | Ptr
    [@@deriving bin_io, compare, sexp, variants]
end

type s = S.t
  [@@deriving bin_io, compare, sexp]

module Pat = struct
  type t =
    | Never
    | Call of id * v * v list
    | Jump of [`call | `goto | `ret | `exn | `jmp] * v * v
    | Move of v * v
    | Load of v * v
    | Wild of v
    | Store of v * v
    [@@deriving bin_io, compare, sexp, variants]
end

type pat = Pat.t
  [@@deriving bin_io, compare, sexp]
