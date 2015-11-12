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

(** variable sort ::= reg | ptr
    variables of type reg represent values stored in registers.
    variables of type ptr represent values stored at addresses,
    that are stored in a given register, e.g.,

    [{
      reg x, *p s.t. x=R0, p=R0;
      x = malloc()
      p = fgets()
    }]

    In case of malloc, we're interested in the pointer itself,
    not in the value, that is stored at address [x] (in fact, malloc
    usually doesn't touch anything at this address).

    In case of `fgets()` we're not interested in the value of register
    [R0], but we're interested in data, that is stored at address
    [R0].
*)
module S : sig
  include module type of S with type t = S.t
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
  (** {2 Keywords}  *)

  type that val that : that
  type such val such : such
  type vars val vars : vars

  (** {2 Type system}  *)

  type dec = v * s

  val reg : v -> dec
  val ( * ) : (v -> dec) -> v -> dec


  val define : id -> rule list -> vars -> dec list -> such -> that -> constr list -> defn
  val rule : id -> pat list -> pat list -> rule


  (** {2 Constraints}  *)
  val (/) : v -> v -> constr
  val (=) : v -> var -> constr
  val forall : v -> such -> that -> id -> constr


  (** {2 Term patterns}  *)

  type rhs

  (** {3 Definitions}  *)
  val (:=) : v -> rhs -> pat
  val use : v -> rhs
  val any : v -> pat

  (** {3 Calls}  *)
  val call : id -> v list -> rhs

  (** {3 Jumps}  *)
  val case : v -> (v -> v -> pat) -> v -> pat
  val goto : v -> v -> pat
  val ret : v -> v -> pat
  val jmp : v -> v -> pat
  val sub : id -> v list -> pat


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
