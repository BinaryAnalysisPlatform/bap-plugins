open Core_kernel.Std
open Bap.Std

module Id = String

type id = Id.t
with bin_io, compare, sexp


module Constr = struct
  type t = {
    exp : exp;
    value : word option
  } with bin_io, compare, sexp, fields
end

type constr = Constr.t
with bin_io, compare, sexp


module V = struct
  type t = {
    id : id;
    constr : constr option
  } with bin_io, compare, fields, sexp
end

type v = V.t with bin_io, compare, sexp

module E = struct
  type t =
    | Reg of v
    | Ptr of v * v
  with bin_io, compare, sexp, variants
end

type e = E.t
with bin_io, compare, sexp

module P = struct
  type t = {
    base : v;
    off  : v;
  } with bin_io, compare, fields, sexp
end

type p = P.t
with bin_io, compare, sexp

module Rule = struct
  type t =
    | Pred of id * v list
    | Call of id * e list * e list
    | Jump of v * v
    | Move of v * v
    | Load of v * p
    | Store of v * p
    | Dep of v * v
  with bin_io, compare, sexp, variants
end

type rule = Rule.t
with bin_io, compare, sexp

module Judgement = struct
  type t = {
    name : string;
    premises : rule list;
    conclusion : rule list;
  } with bin_io, compare, fields, sexp
end

type judgement = Judgement.t
with bin_io, compare, sexp

module Definition = struct
  type t = {
    name : string;
    judgements : judgement list
  } with bin_io, compare, fields, sexp

end

type definition = Definition.t
with bin_io, compare, sexp

type spec = definition list
