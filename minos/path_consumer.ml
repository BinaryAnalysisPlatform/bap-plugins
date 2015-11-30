open Core_kernel.Std
open Bap.Std
open Check
open Util

let consume sub_path (check : Check.t) (ctxt : Check.ctxt) =
  (** Priority type may change, so stick with pattern matching *)
  (** Without SSA, dependence matching fails and so does my check.
      comment out for now*)
  try
    match Util.timeout ~secs:3 ~f:check.run ~x:ctxt with
    | p -> Output.path_priority ctxt.path_dir ctxt.count p
  with
  | Timeout -> Format.printf "TIMEOUT!\n%!"; ()
