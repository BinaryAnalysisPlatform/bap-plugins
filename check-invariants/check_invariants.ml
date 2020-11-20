open Core_kernel
open Bap.Std
open Bap_main
open Graphlib.Std

let sexp_of_tid tid = Sexp.Atom (Tid.name tid)
type block = Addr of addr | Tid of tid
[@@deriving sexp]

type error = Duplicating of tid
           | Unreachable of string * block
[@@deriving sexp]
exception Broken of error [@@deriving sexp]

let broken err = raise (Broken err)

let term_identifiers_are_unique program =
  let visitor = object inherit [Tid.Set.t] Term.visitor
    method! enter_term _ t visited =
      let tid = Term.tid t in
      if Set.mem visited tid
      then broken (Duplicating tid);
      Set.add visited tid
  end in
  (visitor#run program Tid.Set.empty : Tid.Set.t) |> ignore

let symtab_is_well_formed symtab =
  Symtab.to_sequence symtab |>
  Seq.iter ~f:(fun (name,entry,cfg) ->
      Graphlib.depth_first_search (module Graphs.Cfg) cfg
        ~start:entry ~init:()
        ~start_tree:(fun start () ->
            if not @@ Block.equal start entry
            then broken (Unreachable (name, Addr (Block.addr start)))))

let program_is_well_formed prog =
  Term.enum sub_t prog |>
  Seq.iter ~f:(fun sub -> match Term.first blk_t sub with
      | None -> ()
      | Some entry ->
        let cfg =
          Graphs.Tid.Node.remove Graphs.Tid.start
            (Sub.to_graph sub) in
        let entry = Term.tid entry in
        Graphlib.depth_first_search (module Graphs.Tid) cfg
          ~start:entry ~init:()
          ~start_tree:(fun start () ->
              if not @@ Tid.equal start entry
              then broken (Unreachable (Sub.name sub, Tid start))))




let check_invariants proj =
  symtab_is_well_formed (Project.symbols proj);
  term_identifiers_are_unique (Project.program proj);
  program_is_well_formed (Project.program proj)


let () = Extension.declare @@ fun ctxt ->
  Project.register_pass' check_invariants;
  Ok ()
