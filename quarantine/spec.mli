open Core_kernel.Std
open Bap.Std
open Spec_types


module Constr : sig
  include module type of Constr with type t = Constr.t
  include Regular with type t := t
end

module V : sig
  type t = V.t
  val create : string -> t
  include Regular with type t := t
end

module E : sig
  include module type of E with type t = E.t
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

module Defn : sig
  include module type of Defn with type t = Defn.t
  include Regular with type t := t
end

module Spec : sig
  type t = defn list
  with bin_io, compare, sexp
  include Regular with type t := t
end


module Language : sig

  val (/) : v -> v -> constr
  val (=) : v -> var -> constr
  val (:=) : v -> (v -> pat) -> pat
  val jmp : v -> v -> pat
  val sub : id -> v list -> v -> pat
  val call : id -> v list -> pat
  val goto : v -> v -> pat
  val ret : v -> v -> pat

  val term : v -> v -> pat
  val use : v -> pat

  val case : v -> (v -> v -> pat) -> v -> pat

  val such : v -> (id -> v -> constr) -> id -> constr
  val that : id -> v -> constr

  val define : id -> constr list -> rule list -> defn
  val rule : id -> pat list -> pat list -> rule


  val a : v
  val b : v
  val c : v
  val d : v
  val p : v
  val q : v
  val r : v
  val s : v
  val t : v
  val u : v
  val v : v
  val x : v
  val y : v
  val z : v

  val a' : v
  val b' : v
  val c' : v
  val d' : v
  val p' : v
  val q' : v
  val r' : v
  val s' : v
  val t' : v
  val u' : v
  val v' : v
  val x' : v
  val y' : v
  val z' : v
end
