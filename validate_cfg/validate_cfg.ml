open Core_kernel.Std
open Bap.Std

let main proj =
  let prog = Project.program proj in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      let cfg = Sub.to_graph sub in
      let name = Sub.name sub in
      match Term.first blk_t sub with
      | None ->
        eprintf "sub %s is empty\n%!" name;
      | Some blk ->
        let n = Graphlib.fold_reachable (module Graphlib.Tid.Tid)
            ~init:0 ~f:(fun s _ -> s + 1) cfg (Term.tid blk) in
        if n <> Term.length blk_t sub
        then eprintf "sub %s has unreachable blocks\n%!" name)

let () = Project.register_pass' main
