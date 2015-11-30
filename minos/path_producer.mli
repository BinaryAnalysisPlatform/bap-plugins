open Bap.Std
open Options
open Ctxt
open Trim

(** Start the path runner *)
(** project, options, path_dir, trim_dir, max_depth, check *)
val produce : project -> options -> string -> string -> trim -> Check.t -> Ctxt.t
