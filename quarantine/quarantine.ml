open Core_kernel.Std
open Bap.Std
open Format
open Spec
open Specification
let k = 500


let black_addr = Addr.Set.of_list [
    Addr.of_int32 0x00051d30l;
  ]

let black_term = [
  "%16e"
] |> List.map ~f:Tid.from_string_exn
  |> Tid.Set.of_list

let is_interesting_sub sub = true

type mapper = {map : 'a. 'a term -> 'a term}

let mark_if_visited ctxt =
  let map t =
    if Set.mem ctxt#visited (Term.tid t)
    then Term.set_attr t foreground `green else t in
  {map}

let mark_if_tainted (ctxt : Main.result) =
  let map t =
    let taint taint set t =
      if Map.is_empty set then t else Term.set_attr t taint set in
    let regs = ctxt#tainted_regs (Term.tid t) in
    let ptrs = ctxt#tainted_ptrs (Term.tid t) in
    let t = taint Taint.regs regs t in
    let t = taint Taint.ptrs ptrs t in
    if Map.is_empty regs && Map.is_empty ptrs
    then t else
      Term.set_attr t foreground `red in
  {map}

let mark_black_terms = {
  map = fun t ->
    let mark () = 
      Term.set_attr t color `black in
    if Set.mem black_term (Term.tid t) 
    then mark ()
    else match Term.get_attr t Disasm.insn_addr with
    | Some a when Set.mem black_addr a -> mark ()
    | _ -> t
}

let is_seeded t =
  Term.has_attr t Taint.reg ||
  Term.has_attr t Taint.ptr

let mark_if_seeded =
  let map t =
    if is_seeded t
    then Term.set_attr t background `red else t in
  {map}

let seed taint tid =
  let map t =
    if Term.tid t = tid
    then Term.set_attr t taint tid else t in
  {map}

let colorize taint c tid = { map = fun t ->
    if Term.has_attr t taint
    then Term.set_attr t color c else t
  }

let unseed_if_non_visited vis = {
  map = fun t ->
    if Set.mem vis (Term.tid t) then t
    else Term.del_attr (Term.del_attr t Taint.ptr) Taint.reg
}

let marker_of_markers markers =
  let map t =
    List.fold markers ~init:t ~f:(fun t {map} -> map t) in
  {map}

let map_terms {map} prog  =
  Term.map sub_t prog ~f:(fun sub ->
      map sub |>
      Term.map arg_t ~f:map |>
      Term.map blk_t ~f:(fun blk ->
          map blk |>
          Term.map phi_t ~f:map |>
          Term.map def_t ~f:map |>
          Term.map jmp_t ~f:map))


let contains_seed sub =
  let has t p = Term.enum t p |> Seq.exists ~f:is_seeded in
  has arg_t sub || Term.enum blk_t sub |> Seq.exists ~f:(fun blk ->
      has phi_t blk || has def_t blk)

let seeded callgraph subs =
  let callers sub =
    Graphlib.fold_reachable (module Graphlib.Callgraph) callgraph
      ~rev:true ~init:Tid.Set.empty ~f:Set.add (Term.tid sub) in
  Seq.filter subs ~f:contains_seed |>
  Seq.fold ~init:Tid.Set.empty ~f:(fun subs sub ->
      Set.union subs @@ callers sub)

let tids_of_sub sub =
  let terms t p =
    Term.enum t p |> Seq.map ~f:Term.tid |> Seq.to_list_rev in
  let (++) = List.rev_append in
  let init = terms arg_t sub in
  Term.enum blk_t sub |> Seq.fold ~init ~f:(fun sum blk ->
      terms phi_t blk ++
      terms def_t blk ++
      terms jmp_t blk ++ sum)

type stats = {
  sub_count : int;
  sub_total : int;
  visited : Tid.Set.t;
  terms : Tid.Set.t;
}

let stats sub_total = {
  sub_count = 0;
  sub_total;
  visited = Tid.Set.empty;
  terms = Tid.Set.empty;
}

let percent (x,y) =
  if y = 0 then 0
  else Int.of_float (100. *. (float x /. float y))

let pp_ratio ppf (x,y) =
  fprintf ppf "[%d/%d] %3d%%" x y (percent (x,y))

let pp_progressbar ppf {sub_count=x; sub_total=y} =
  fprintf ppf "%a" pp_ratio (x,y)

let pp_coverage ppf {visited; terms} =
  let x = Set.length visited in
  let y = Set.length terms in
  pp_ratio ppf (x,y)

let add_list ss xs =
  Set.union ss @@ Tid.Set.of_list xs

let entered_sub stat sub = {
  stat with
  sub_count = stat.sub_count + 1;
  terms = add_list stat.terms (tids_of_sub sub)
}
let visited_sub stat res = {
  stat with
  visited = Set.union stat.visited res#visited
}

let main proj =
  printf "* Specification@.%a" Spec.pp spec;
  let prog = Project.program proj |>
             map_terms mark_black_terms in
  let prog = Tainter.seed spec prog in
  let proj = Project.with_program proj prog in
  let callgraph = Program.to_graph prog in
  let subs = Term.enum sub_t prog |>
             Seq.filter ~f:is_interesting_sub |>
             (* Seq.map ~f:Term.tid |> Seq.to_list_rev |> Tid.Set.of_list in *)
             seeded callgraph in
  let proj,stat =
    Term.enum sub_t prog |>
    Seq.filter ~f:(fun sub -> Set.mem subs (Term.tid sub)) |>
    Seq.fold ~init:(proj,stats (Set.length subs))
      ~f:(fun (proj,stat) sub ->
          let stat = entered_sub stat sub in
          eprintf "%-40s %a\r%!"
            (Sub.name sub) pp_progressbar stat;
          let ctxt = Main.run proj k (`Term (Term.tid sub)) in
          let mark = marker_of_markers [
              mark_if_visited ctxt;
              mark_if_tainted ctxt;
              mark_if_seeded;
            ] in
          let prog = Project.program proj |> map_terms mark in
          let stat = visited_sub stat ctxt in
          Project.with_program proj prog, stat) in
  eprintf "Coverage: %a@." pp_coverage stat;
  eprintf "@[<v>Solving...@;";
  let prog = Project.program proj |>
             map_terms (unseed_if_non_visited stat.visited) in
  let tainter = Tainter.reap prog in
  let state = State.create spec tainter in
  let state = Solver.run state prog in
  let sol = State.solution state spec in
  List.iter (Spec.defns spec) ~f:(fun defn ->
      printf "@[* %s@." (Defn.name defn);
      printf "@[** find %s@." (Defn.name defn);
      printf "%a" (Solution.pp_sat defn) sol;
      printf "@]";
      printf "@[** assert %s@." (Defn.name defn);
      printf "%a" (Solution.pp_unsat defn) sol;
      printf "@]@]");
  proj

let () = Project.register_pass "quarantine" main
