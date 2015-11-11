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
  val define : id -> rule list -> constr list -> defn
  val rule : id -> pat list -> pat list -> rule

  (** {2 Type system}  *)

  type typ = v -> e

  val reg : typ
  val ( * ) : typ -> typ

  (** {2 Constraints}  *)
  type that
  val that : that
  val (/) : v -> v -> constr
  val (=) : v -> var -> constr
  val such : v -> that -> id -> constr


  (** {2 Term patterns}  *)

  type rhs

  (** {3 Definitions}  *)
  val (:=) : e -> rhs -> pat
  val use : e -> rhs
  val any : v -> pat

  (** {3 Calls}  *)
  val sub : id -> e list -> rhs
  val call : id -> e list -> pat

  (** {3 Jumps}  *)
  val case : v -> (v -> v -> pat) -> v -> pat
  val goto : v -> v -> pat
  val ret : v -> v -> pat
  val jmp : v -> v -> pat


  (** {2 Predicates}  *)

  val is_marked : id
  val is_black : id
  val is_red : id
  val is_green : id
  val is_yellow : id
  val is_blue : id
  val is_magenta : id
  val is_cyan : id
  val is_white : id


  (** {2 Predefined Variables} *)
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
