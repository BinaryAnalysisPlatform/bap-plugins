open Bap.Std
open Format
open Options

type t = {
  trace : tid list;
  addr : addr;
  use_tid : tid;
  free_tid : tid;
  alloc_tid : tid
}

val callstack : Program.t -> t -> string list
val print_callstack : Project.t -> Options.t -> t -> unit
val print_trace : Options.t -> t -> unit
val to_string : t -> string
val pp : formatter -> t -> unit
val pps : unit -> t -> string
