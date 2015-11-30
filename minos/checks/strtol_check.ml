(** Spec:
    Emit 0 for paths where
      a) arg1 is not a const
      b) there is no data dependence of arg1 on on jumps
*)

open Bap.Std
open Core_kernel.Std
open Policy
open Options
open Args
open Check
open Check_tmpl

module P = Policy.Predicate

(** CHECK *)
let max_paths = 300

let should_produce' args sink_blk =
  match args with
  | {arg1 = Some (tid,_,_)} ->
    P.arg_is_not_const sink_blk tid
  | _ -> false

(** CHECK *)
let check_path' inter_dependence path_attrs =
  match path_attrs.args with
  | {arg1 = Some _} when
      P.size_eq_0 inter_dependence -> 0
  | _ -> 5

let check_path sink_blk sub_path (ctxt : Check.ctxt) path_attrs =
  let module P = Policy.Predicate in

  (** Calculate inter dependence with arg1 *)
  let inter_dependence,arg_deps = match path_attrs.args with
    | {arg1 = Some (_,_,arg_deps)} ->
      Dependence.inter_dep arg_deps path_attrs.jmp_deps,arg_deps
    | _ -> Seq.empty,Seq.empty in

  let jmp_tids = Check_tmpl.get_jmp_tids sub_path in
  Dependence.output sub_path arg_deps path_attrs.jmp_deps
    inter_dependence jmp_tids ctxt;
  check_path' inter_dependence path_attrs

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
  if ctxt.num_paths < max_paths && ctxt.num_paths > 0 then
    let trim = ctxt.trim in
    let sink_blk = Term.find blk_t ctxt.trim.trim_sub trim.sink_tid |>
                   Util.val_exn in
    let args = Check_tmpl.infer_args ctxt sink_blk in
    should_produce' args sink_blk
  else
    false

let check : (Check.t) =
  {should_produce; run;
   reverse=false; max_depth=(-1);
  sample=(-1); timeout=(-1)}
