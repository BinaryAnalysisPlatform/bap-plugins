open Core_kernel.Std
open Bap.Std
open Format
open Spec

let seed spec proj =
  let prog = Project.program proj in
  Project.with_program proj (Tainter.seed spec prog)

let solve spec proj =
  let prog = Project.program proj in
  let tainter = Tainter.reap prog in
  let state = State.create spec tainter in
  let state = Solver.run state prog in
  let sol = State.solution state spec in
  printf "* Specification@.%a" Spec.pp spec;
  List.iter (Spec.defns spec) ~f:(fun defn ->
      printf "@[* %s@." (Defn.name defn);
      printf "@[** find %s@." (Defn.name defn);
      printf "%a" (Solution.pp_sat defn) sol;
      printf "@]";
      printf "@[** assert %s@." (Defn.name defn);
      printf "%a" (Solution.pp_unsat defn) sol;
      printf "@]@]@.")

let () =
  let spec = Specification.spec in
  Project.register_pass  ~name:"seed"  (seed spec);
  Project.register_pass' ~name:"solve" (solve spec)
