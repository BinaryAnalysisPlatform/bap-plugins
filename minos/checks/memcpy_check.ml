(** Spec:
    Emit 0 for paths where
      a) there is no data dependence of arg3 on on jumps
      b) the dependence span is 1 (subsumes a)
      c) the path does not contain an ite
      d) the path does not contain a call to malloc (which would imply that
         memory is allocated to the right size before the memcpy)
*)

open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy

module P = Policy.Predicate

(** CHECK *)
let max_paths = 30

let should_produce' args sink_blk =
  match args with
  | {arg3 = Some (tid,_,_)} ->
    P.arg_is_not_const sink_blk tid
  | _ -> false

(** CHECK *)
let check_path' inter_dependence path_attrs sub_path sink_blk =
  match path_attrs with
  | {args = {arg3 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      Policy.dep_blk_span dep sub_path = 1 &&
      not (P.contains_ite sub_path dep) &&
      not (P.contains_calls ["@.malloc"; "@malloc"] sub_path)
    -> 0
  | {args = {arg3 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      Policy.dep_blk_span dep sub_path < 3 &&
      not (P.contains_ite sub_path dep) &&
      not (P.contains_calls ["@.malloc"; "@malloc"] sub_path)
    -> 1
  | {args = {arg3 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      num_blks < 5
    -> 2
  (** any number of blocks *)
  | {args = {arg3 = Some (_,_,dep)}; jmp_deps} when
      P.size_eq_0 inter_dependence -> 3
  | {args = {arg3 = Some (tid,_,_)}} when P.arg_is_const_0 sink_blk tid -> 5
  | _ -> 4

let check_path sink_blk sub_path (ctxt : Check.ctxt) path_attrs =
  (** AUX DATA *)
  let inter_dependence,arg_deps = match path_attrs.args with
    | {arg3 = Some (_,_,arg_deps)} ->
      Dependence.inter_dep arg_deps path_attrs.jmp_deps,arg_deps
    | _ -> Seq.empty,Seq.empty in

  let jmp_tids = Check_tmpl.get_jmp_tids sub_path in
  (** Output *)
  Dependence.output sub_path arg_deps path_attrs.jmp_deps
    inter_dependence jmp_tids ctxt;

  check_path' inter_dependence path_attrs sub_path sink_blk

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
