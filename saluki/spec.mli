open Core_kernel.Std
open Bap.Std
open Regular.Std
open Spec_types


module Constr : sig
  include module type of Constr with type t = Constr.t
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

type constr = Constr.t [@@deriving bin_io, compare, sexp]
type id = string [@@deriving bin_io, compare, sexp]
type v    = V.t    [@@deriving bin_io, compare, sexp]
type s    = S.t    [@@deriving bin_io, compare, sexp]
type pat  = Pat.t  [@@deriving bin_io, compare, sexp]


module Rule : sig
  type t
  val name : t -> string
  val premises : t -> pat list
  val conclusions : t -> pat list
  include Regular with type t := t
end

type rule = Rule.t [@@deriving bin_io, compare, sexp]

module Defn : sig
  type t
  val name : t -> string
  val vars : t -> (v * s) list
  val ivars : t -> V.Set.t
  val constrs : t -> constr list
  val rules : t -> rule list
  include Regular with type t := t
end

type defn = Defn.t [@@deriving bin_io, compare, sexp]

module Spec : sig
  type t [@@deriving bin_io, compare, sexp]
  val defns : t -> defn list
  val filter : t -> f:(defn -> bool) -> t
  include Regular with type t := t
end

type spec = Spec.t [@@deriving bin_io, compare, sexp]

module Language : sig
  (** {2 Keywords}  *)

  type that val that : that
  type such val such : such
  type vars val vars : vars

  (** {2 Type system}  *)

  type dec = V.t * s

  val reg : V.t -> dec
  val ( * ) : (V.t -> dec) -> V.t -> dec


  val specification : defn list -> spec
  val define : id -> rule list -> vars -> dec list -> such -> that -> constr list -> defn
  val rule : id -> pat list -> pat list -> rule


  (** {2 Constraints}  *)
  val (/) : V.t -> V.t -> constr
  val (=) : V.t -> var -> constr
  val forall : V.t -> such -> that -> id -> constr


  (** {2 Term patterns}  *)

  type rhs

  (** {3 Definitions}  *)
  val (:=) : V.t -> rhs -> pat
  val use : V.t -> rhs
  val any : V.t -> pat
  val never : pat

  (** {3 Calls}  *)
  val call : id -> V.t list -> rhs

  (** {3 Jumps}  *)
  val case : V.t -> (V.t -> V.t -> pat) -> V.t -> pat
  val goto : V.t -> V.t -> pat
  val ret : V.t -> V.t -> pat
  val jmp : V.t -> V.t -> pat
  val sub : id -> V.t list -> pat


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
  val a : V.t
  val b : V.t
  val c : V.t
  val d : V.t
  val p : V.t
  val q : V.t
  val r : V.t
  val s : V.t
  val t : V.t
  val u : V.t
  val v : V.t
  val x : V.t
  val y : V.t
  val z : V.t

  val _' : V.t
  val a' : V.t
  val b' : V.t
  val c' : V.t
  val d' : V.t
  val p' : V.t
  val q' : V.t
  val r' : V.t
  val s' : V.t
  val t' : V.t
  val u' : V.t
  val v' : V.t
  val x' : V.t
  val y' : V.t
  val z' : V.t
end
