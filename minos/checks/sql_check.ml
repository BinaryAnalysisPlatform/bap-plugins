open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy

module P = Policy.Predicate

(** CHECK *)
let max_paths = 10000

let should_produce' ctxt args sink_blk =
  true

(** CHECK *)
let check_path' path_attrs sub_path =
  let p = Policy.Predicate.contains_call in
  let pred = p "@_ZNSs6appendERKSs" sub_path ||
             p "@_ZNSs6appendEPKcj" sub_path in
  match pred with
  | true when path_attrs.num_blks < 10 -> 0
  | true when path_attrs.num_blks < 20 -> 1
  | true when path_attrs.num_blks < 50 -> 2
  | true -> 3
  | false -> 5

let check_path sink_blk sub_path (ctxt : Check.ctxt) path_attrs =
  (** AUX DATA *)
  Dependence.output sub_path Seq.empty Seq.empty Seq.empty Seq.empty ctxt;
  check_path' path_attrs sub_path

(** Infer arguments from sink blk, if sink blk is the sink tid. Then
    check the path. *)
let run ctxt =
  let sink_blk = Term.last blk_t ctxt.sub_path |> Util.val_exn in
  let args = Check_tmpl.infer_args_with_deps ctxt sink_blk in
  let jmp_deps = Check_tmpl.get_jmp_deps ctxt.sub_path in
  let num_blks = Term.enum blk_t ctxt.sub_path |> Seq.length in
  let path_attr = {args; jmp_deps; num_blks} in
  check_path sink_blk ctxt.sub_path ctxt path_attr

let should_produce ctxt =
  let open Trim in
  (** AUX DATA *)

  let test =
    let trim = ctxt.trim in
    let sink_blk = Term.find blk_t ctxt.trim.trim_sub trim.sink_tid |>
                   Util.val_exn in
    let args = Check_tmpl.infer_args ctxt sink_blk in
    should_produce' ctxt args sink_blk in
  if ctxt.num_paths < max_paths && ctxt.num_paths > 0 && test then
    true
  else
    false

let check : (Check.t) =
  {should_produce; run;
   reverse=false; max_depth=(-1);
  sample=(-1);timeout=(-1)}
