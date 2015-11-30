open Core_kernel.Std
open Bap.Std

(** sub, with summary information *)
type 'a t =
  {subroutine : sub term;
   data : 'a}

let to_table seq =
  let table = Sub.Table.create () in
  Seq.iter seq ~f:(fun summary ->
      Sub.Table.add_exn table ~key:summary.subroutine ~data:summary.data);
  table

(** populate summary information for subroutines in the callgraph,
    bottom-up (post-order). Assume no recursive structures for
    now. *)
(** Graphlib.Ir -> nodes are always blks. *)
let summarize_bottom_up callgraph ~f =
  Graphlib.postorder_traverse (module Graphlib.Callgraph) callgraph |>
  (** produce a seq of summaries *)
  Seq.filter_map ~f |> to_table
