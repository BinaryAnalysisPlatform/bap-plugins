open Core_kernel
open Bap.Std
open Spec

(** constraints generated by pattern matchers  *)
type t =
  | Eql of v * var    (** v bounds to var         *)
  | Any of t list     (** disjunction of matches  *)
  | All of t list     (** conjunction of matches  *)
[@@deriving variants]


(** [Any []] is a bot value (never matches)  *)
val bot : t

(** [All []] is a top value (always matches) *)
val top : t


class matcher : object
  method arg : arg term -> pat -> t
  method phi : phi term -> pat -> t
  method def : def term -> pat -> t
  method jmp : jmp term -> pat -> t
end

(** returns a list of matcher that implements
    all patterns. *)
val patterns : program term -> matcher list

val pp : Format.formatter -> t -> unit
