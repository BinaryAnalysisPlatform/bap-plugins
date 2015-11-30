open Core_kernel.Std
open Bap.Std

(** If sub is a path with constants folded to goto term constants,
    this will resolve the goto's to the correct jump target, if it
    exists *)
let resolve_calls project sub =
  let symtab = Project.symbols project in
  Term.map blk_t sub ~f:(fun blk ->
      Term.map jmp_t blk ~f:(fun jmp ->
          match Jmp.kind jmp with
          | Call c -> (match Call.target c with
              | Indirect (Bil.Int addr) ->
                (Symtab.find_by_start symtab addr |> function
                  | Some fn ->
                    let fn_target = Symtab.name_of_fn fn in
                    let t = Tid.(!("@"^fn_target)) in
                    Jmp.create_call ~tid:(Term.tid jmp)
                      ~cond:(Jmp.cond jmp)
                      (Call.with_target c (Label.direct t))
                  | None -> jmp)
              | _ -> jmp)
          | _ -> jmp))
