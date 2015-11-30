open Bap.Std
open Check
open Core_kernel.Std

type arg = tid * Def.t * tid seq

(* 1 indexed *)
type args = {arg1 : arg option;
             arg2 : arg option;
             arg3 : arg option}

type path_attrs = {args : args;
                   jmp_deps : tid seq;
                   num_blks : int}

let get_jmp_tids sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:Seq.empty ~f:(fun acc blk ->
      Term.enum jmp_t blk |> Seq.fold ~init:acc ~f:(fun acc jmp ->
          (Term.tid jmp) ^:: acc))

(** Create an arg type if it exists in this blk *)
let infer_arg (ctxt : ctxt) blk arg : arg option =
  let inferred_arg = Args.infer ctxt.project blk arg in
  match Seq.hd inferred_arg with
  | Some (tid,def) ->
    Some (tid,def,Seq.empty)
  | None -> None (** No def for this arg in blk *)

(** do not populate deps *)
let infer_args ctxt sink_blk : args =
  let (!) = infer_arg ctxt sink_blk in
  let tmp =
    List.map ["arg1"; "arg2"; "arg3"] ~f:(!) in
  {arg1 = (List.nth_exn tmp 0);
   arg2 = (List.nth_exn tmp 1);
   arg3 = (List.nth_exn tmp 2)}

let get_jmp_deps sub_path =
  let (!) = Seq.map ~f:Term.tid in
  let jmp_tids = get_jmp_tids sub_path in
  !(Seq.concat_map jmp_tids ~f:(Dependence.jmp_dep sub_path))

(** Fill in the data dependence for an arg *)
let populate_deps sub_path arg : arg =
  let (!) = Seq.map ~f:Term.tid in
  let tid,def,deps = arg in
  let deps = !(Dependence.def_dep sub_path tid) in
  (tid,def,deps)

(** TODO, use infer_args and simplify *)
let infer_args_with_deps ctxt sink_blk =
  let (!) = infer_arg ctxt sink_blk in
  let tmp =
    List.map ["arg1"; "arg2"; "arg3"] ~f:(!) |>
    List.map ~f:(fun x ->
        Option.(x >>= fun x -> Some (populate_deps ctxt.sub_path x))) in
  {arg1 = (List.nth_exn tmp 0);
   arg2 = (List.nth_exn tmp 1);
   arg3 = (List.nth_exn tmp 2)}
