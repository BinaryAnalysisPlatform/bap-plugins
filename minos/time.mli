(* Wrap a function with lazy and pass to time, with a tag, to time the
   operation *)
val time : string -> 'a lazy_t -> 'a
