let doc = {|
# DESCRIPTION

Saluki is a proof-based verificiation engine that uses a
simple embedded DSL for property specification. Saluki relies on an
external taint propagation engine for data flow facts. Based on these
facts Saluki will search for models for each specified property.

For more information about Saluki please refer to its homepage [1] and
paper [2].


[1]: https://github.com/BinaryAnalysisPlatform/bap-plugins/tree/master/saluki
[2]: https://www.ndss-symposium.org/wp-content/uploads/2018/07/bar2018_19_Gotovchits_paper.pdf

|}

open Core_kernel
open Bap.Std
open Bap_main
open Format
open Spec

let taint spec proj =
  let prog = Project.program proj in
  Project.with_program proj (Tainter.seed spec prog)

let pp_models sol defn =
  printf "@[models %s@." (Defn.name defn);
  printf "%a" (Solution.pp_sat defn) sol

let filter checks defs =
  Spec.filter defs ~f:(fun defn ->
      List.exists checks ~f:(fun pattern ->
          String.substr_index (Defn.name defn) ~pattern <> None))

let has_visited blk =
  Term.has_attr blk Term.visited

let compute_coverage prog =
  Term.enum sub_t prog |>
  Seq.fold ~init:(0,0) ~f:(fun state sub ->
      Term.enum blk_t sub |>
      Seq.fold ~init:state ~f:(fun (visited,total) blk ->
          if not (has_visited blk)
          then printf "%a" Blk.pp blk;
          if has_visited blk then (visited+1,total+1)
          else (visited,total+1))) |> function
  | (0,0) -> 0,0,0
  | (v,t) ->
    v,t,Float.(to_int @@ round @@ (of_int v /. of_int t) *. 100.)

let pp_coverage ppf prog =
  let visited,total,percentage = compute_coverage prog in
  Format.fprintf ppf "[%d/%d] %3d%%" visited total percentage


let solve models spec coverage proj =
  let prog = Project.program proj in
  let tainter = Tainter.reap prog in
  let state = State.create spec tainter in
  let state = Solver.run state prog in
  let sol = State.solution state spec in
  printf "* Specification@.%a" Spec.pp spec;
  List.iter (Spec.defns spec) ~f:(fun defn ->
      printf "@[assert %s@." (Defn.name defn);
      printf "%a" (Solution.pp_unsat defn) sol;
      printf "@]";
      if models then pp_models sol defn);
  if coverage then printf "Coverage: %a@\n" pp_coverage prog;
  Project.with_program proj (Solution.annotate sol prog)

(* Command line interface *)

open Extension.Configuration
open Extension.Type

let check = parameter (some (list string)) "check"
    ~doc:"Check the specified list of properties. The list
    may contain a full property name, or just some substring. For
    example,if $(b,malloc) is specified, then all properties that
    contain $(b,malloc) in their name will be checked."

let models = flag "print-models"
    ~doc:"Output found models for each property. Usefull for testing
       and debugging."

let print_coverage = flag "print-coverage"
    ~doc:"Prints the percentage of visited blocks"

let () = Extension.declare ~doc @@ fun ctxt ->
  let spec = match get ctxt check with
    | None -> Specification.spec
    | Some checks -> filter checks (Specification.spec) in
  let coverage = get ctxt print_coverage in
  let models = get ctxt models in
  Project.register_pass ~name:"solve" (solve models spec coverage);
  Project.register_pass ~deps:["callsites"] ~name:"taint"  (taint spec);
  Project.register_pass' ignore
    ~deps:["saluki-taint"; "propagate-taint"; "saluki-solve"];
  Ok ()
