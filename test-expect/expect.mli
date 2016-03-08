(** Given a sequence of expected strings [E] and test corpora [S], we
    want to ensure, that each expected string matches at least one
    substring in a testing corpora, that is not matched by other
    expected string.

    This is a Maximum Bipartile Matching problem. First we find a MBP
    solution, and if in this solution all persons got a job, then we
    are satisfied, otherwise we give a set of expectations, that were
    not satisfied.

    An example, may clarify the problem. Given a following expectation
    specification: [x;x;y], we will any input that has at least two
    [x] and at least one [y].
*)

type t
type misses

(** [create regexp] takes a list of POSIX regular expressions
    and converts it into an expectation *)
val create : string list -> t


(** [all_matches expectation data] checks that provided list of
    strings [data] satisfies given [expectation] *)
val all_matches : t -> string list -> [`Yes | `Missed of misses]

(** [pp_misses ppf misses] prints missed expectation into a given
    formatter. *)
val pp_misses : Format.formatter -> misses -> unit
