open Core_kernel.Std
open Bap.Std
open Format
open Spec
include Self()

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

let solve models spec proj =
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
  Project.with_program proj (Solution.annotate sol prog)

let main models checks () =
  let spec = match checks with
    | None -> Specification.spec
    | Some checks -> filter checks (Specification.spec) in
  Option.is_some checks, spec,models


module Cmdline = struct
  open Cmdliner
  let taint =
    let doc = "Run taint pass" in
    Arg.(value & flag & info ["taint"] ~doc)
  let solver =
    let doc = "Run saluki solver" in
    Arg.(value & flag & info ["solver"] ~doc)
  let saluki =
    let doc = "Taint, then propagate taint, then run the solver" in
    Arg.(value & flag & info ["saluki"] ~doc)

  let passes =
    Term.(const (fun _ _ _ -> ()) $taint $solver $saluki)

  let check =
    let doc = "Check the specified list of properties. The list
    may contain a full property name, or just some substring. For
    example,if $(b,malloc) is specified, then all properties that
    contain $(b,malloc) in their name will be checked." in
    Arg.(value & opt (some (list string)) None &
         info ["check"] ~doc)

  let models =
    let doc =
      "Output found models for each property. Usefull for testing
       and debugging." in
    Arg.(value & flag & info ["print-models"] ~doc)

  let man = [
    `S "DESCRIPTION";
    `P "Saluki is a proof base property verification engine. It uses
        a simple DSL that allows to specify program properties. The
        solver will verify that dataflow facts proves or disproves the
        given set of properties."
  ]

  let doc = "fast and stupid property checker"

  let info = Term.info name ~doc ~man
  let args = Term.(const main $models $check $passes),info
end


let () = match Cmdliner.Term.eval ~argv Cmdline.args with
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
  | `Ok (autorun, spec,models) ->
    Project.register_pass ~name:"solve" (solve models spec);
    Project.register_pass ~deps:["callsites"] ~name:"taint"  (taint spec);
    Project.register_pass' ignore ~autorun
      ~deps:[name^"-taint"; "propagate-taint"; name^"-solve"]
