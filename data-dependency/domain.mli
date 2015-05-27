open Core_kernel.Std

(* Sparse bitvector representation *)
type t with sexp

(* Constructor for infinite size domain value, where false is the empty set and
 * true is the universal set *)
val create_infinite : bool -> t

(* Constructor for finite size domain value, given a bit value (false -> 0,
true -> 1) and indexes that should be set to that value *)
val create_finite : int list -> bool -> t

(* Creates an empty domain value *)
val empty : t

(* Creates a universal domain value *)
val univ : t

(* Inverts the representation of a given domain value with optional width. Does
 * not actually change the value of the set, e.g. a = invert a *)
val invert : ?width:int -> t -> t

(* Intersect two domain values *)
val inter : t -> t -> t

(* Union two domain values *)
val union : t -> t -> t

(* Difference of two domain values *)
val diff : t -> t -> t

(* Check whether a domain value is finite *)
val is_finite : t -> bool

(* Get the set bit indices of a domain value *)
val get_indices : t -> Int.Set.t

(* Get the value of a domain value *)
val get_value : t -> bool

(* Fold through the singleton domain values each corresponding to a single set
 * bit in the domain value *)
val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a

(* Pretty-print formatter *)
val pp : Format.formatter -> t -> unit

include Comparable with type t := t
