type t

(** A standard filter for plt calls. [extra] will add strings to the
    filtered set *)
val plt_filter : extra:string list -> t

(** Includes plt_filter, and adds everything matching @_ZNS.* *)
val cpp_filter : extra:string list -> t

(** Given a filter and a string, return whether the string is
    filtered *)
val apply : t -> string -> bool
