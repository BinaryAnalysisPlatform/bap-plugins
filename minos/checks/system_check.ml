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

(** This is before paths. We will fold const on the sub with
    respect to system *)
let should_produce' ctxt args sink_blk =
  match args with
  | {arg1 = Some (tid,_,_)}
    when P.arg_is_not_string ctxt.project sink_blk tid ->
    Output.trim_priority ctxt.trim_dir 0;
    Output.misc (Format.sprintf "Producing for symbolic\n");
    true
  | {arg1 = Some (tid,_,_)} -> (** Arg must be a string *)
    let str =
      Policy.get_arg_as_string ctxt.project sink_blk tid |> Util.val_exn in
    if String.is_substring ~substring:"%" str then (
      Output.misc (Format.sprintf "Found system argument with format specifier\"%S\"\n" str);
      Output.trim_priority ctxt.trim_dir 0)
    else (
      Output.misc (Format.sprintf "Found system argument %S\n" str);
      Output.trim_priority ctxt.trim_dir 1);
    false
  | _ ->
    Output.trim_priority ctxt.trim_dir 5;
    false

(** helper function to resolve constant string arguments to sprintf/snprintf *)
let printf_arg ctxt sub =
  (** get the blk closest at the end of the path that calls sprintf or snprintf *)
  let blks =
    Term.enum blk_t sub |> Seq.fold ~init:Seq.empty ~f:(fun acc blk ->
        if List.exists (Util.calls_of_blk_str blk) ~f:(fun s ->
            s = "@sprintf" || s = "@snprintf") then
          (blk ^:: acc) else acc) in
  match Seq.hd blks with
  | Some blk ->
    (** Infer args *)
    (** return the string it contains *)
    let args = Check_tmpl.infer_args ctxt blk in
    (match args with
     | {arg2 = Some (tid,_,_)}
       when P.arg_is_string ctxt.project blk tid ->
       Policy.get_arg_as_string ctxt.project blk tid
     | {arg3 = Some (tid,_,_)}
       when P.arg_is_string ctxt.project blk tid ->
       Policy.get_arg_as_string ctxt.project blk tid
     | _ -> None)
  | None -> None

(** CHECK *)
(** Path contains a system with symbolic argument. Check if
    it is preceded by a call to sprintf or snprintf *)
let check_path' ctxt inter_dependence path_attrs sub_path sink_blk =
  match path_attrs with
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      Policy.dep_blk_span dep sub_path = 1 &&
      P.contains_calls ["@sprintf"; "@snprintf"] sub_path
    -> (match printf_arg ctxt sub_path with
        | Some s -> Output.misc (Format.sprintf "s[n]printf arg: \"%S\"\n" s); 1
        | None -> 1)
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      P.contains_calls ["@sprintf"; "@snprintf"] sub_path
    -> (match printf_arg ctxt sub_path with
        | Some s -> Output.misc (Format.sprintf "s[n]printf arg: \"%S\"\n" s); 2
        | None -> 2)
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.contains_calls ["@sprintf"; "@snprintf"] sub_path
    -> (match printf_arg ctxt sub_path with
        | Some s -> Output.misc (Format.sprintf "s[n]printf arg: \"%S\"\n" s); 3
        | None -> 3)
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence
    -> 4 (** symbolic, but no strings before? strange. *)
  | _ -> 0

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

  check_path' ctxt inter_dependence path_attrs sub_path sink_blk

(** Infer arguments from sink blk, if sink blk is the sink tid. Then
    check the path. *)
let run ctxt =
  let sink_blk = Term.last blk_t ctxt.sub_path |> Util.val_exn in
  let args = Check_tmpl.infer_args_with_deps ctxt sink_blk in
  let jmp_deps = Check_tmpl.get_jmp_deps ctxt.sub_path in
  let num_blks = Term.enum blk_t ctxt.sub_path |> Seq.length in
  let path_attr = {args; jmp_deps; num_blks} in
  check_path sink_blk ctxt.sub_path ctxt path_attr

let ssa_trim (ctxt : Check.ctxt) =
  let open Trim in
  let arch = Project.arch ctxt.project in
  let ssa_sub = Fold_consts.analyze ~fixsp:false arch ctxt.trim.trim_sub in
  let t = ctxt.trim in
  {ctxt with trim = {t with trim_sub = ssa_sub}}

let should_produce ctxt =
  let open Trim in
  (** AUX DATA *)

  let test =
    let ctxt = ssa_trim ctxt in

    (** Get sink blk after performing SSA *)
    let sink_blk = Term.find blk_t ctxt.trim.trim_sub ctxt.trim.sink_tid |>
                   Util.val_exn in

    let args = Check_tmpl.infer_args ctxt sink_blk in
    should_produce' ctxt args sink_blk in

  if ctxt.num_paths < max_paths && ctxt.num_paths > 0 && test then
    true
  else
    false

let check : (Check.t) =
  {should_produce; run;
   reverse = true;
   max_depth=100;
   sample=(-1); timeout=(-1)}
