open Core_kernel.Std
open Bap.Std
open Spec_types


module Constr : sig
  include module type of Constr with type t = Constr.t
  include Regular with type t := t
end

module V : sig
  include module type of V with type t = V.t
  include Regular with type t := t
end

module E : sig
  include module type of E with type t = E.t
  include Regular with type t := t
end

module Ptr : sig
  include module type of Ptr with type t = Ptr.t
  include Regular with type t := t
end

module Pat : sig
  include module type of Pat with type t = Pat.t
  include Regular with type t := t
end

module Rule : sig
  include module type of Rule with type t = Rule.t
  include Regular with type t := t
end


module Judgement : sig
  include module type of Judgement with type t = Judgement.t
  include Regular with type t := t
end

module Definition : sig
  include module type of Definition with type t = Definition.t
  include Regular with type t := t
end

module Spec : sig
  type t = definition list
  with bin_io, compare, sexp
  include Regular with type t := t
end
