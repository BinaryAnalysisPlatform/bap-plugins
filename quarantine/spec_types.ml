open Core_kernel.Std
open Bap.Std

module Id = String

type id = Id.t
with bin_io, compare, sexp

module V = struct
  type t = id with bin_io, compare, sexp
end
type v = V.t with bin_io, compare, sexp

module Constr = struct
  type t =
    | Dep of v * v
    | Var of v * var
    | Int of v * word
    | Fun of id * v
  with bin_io, compare, sexp, variants
end
type constr = Constr.t
with bin_io, compare, sexp

module E = struct
  type t =
    | Reg of v
    | Ptr of v
  with bin_io, compare, sexp, variants
end

type e = E.t
with bin_io, compare, sexp

module Pat = struct
  type t =
    | Call of id * e list * e list
    | Jump of [`call | `goto | `ret | `exn | `jmp] * v * v
    | Move of v * v
    | Load of v * v
    | Wild of v
    | Store of v * v
  with bin_io, compare, sexp, variants
end

type pat = Pat.t
with bin_io, compare, sexp

module Rule = struct
  type t = {
    name : string;
    premises : pat list;
    conclusion : pat list;
  } with bin_io, compare, fields, sexp
end

type rule = Rule.t
with bin_io, compare, sexp

module Definition = struct
  type t = {
    name : string;
    constrs  : constr list;
    rules : rule list
  } with bin_io, compare, fields, sexp
end

type definition = Definition.t
with bin_io, compare, sexp

type spec = definition list
