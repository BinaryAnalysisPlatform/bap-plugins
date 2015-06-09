open Core_kernel.Std
open Bap.Std
open Format

let transfer term_type blk adder bld =
  Term.to_sequence term_type blk |> Seq.iter ~f:(adder bld)

let builder_of_blk
    ?(same_tid=true)
    ?(copy_phis=false) ?(copy_defs=false) ?(copy_jmps=false) blk =
  let tid = if same_tid then Term.tid blk else Tid.create () in
  let b = Blk.Builder.create ~tid ()
      ~phis:(Term.length phi_t blk)
      ~defs:(Term.length def_t blk)
      ~jmps:(Term.length jmp_t blk) in
  if copy_phis then transfer phi_t blk Blk.Builder.add_phi b;
  if copy_defs then transfer def_t blk Blk.Builder.add_def b;
  if copy_jmps then transfer jmp_t blk Blk.Builder.add_jmp b;
  b

let substitute vars = (object
  inherit Bil.mapper as super
  method! map_sym z =
    match Map.find vars z with
    | Some v -> v
    | None -> super#map_sym z
end)#map_exp

let rename v = Var.(create ~tmp:true (name v) (typ v))

let ssa_blk blk =
  let bld = builder_of_blk ~copy_phis:true ~copy_jmps:true blk in
  Term.to_sequence def_t blk |>
  Seq.fold ~init:Var.Map.empty ~f:(fun subs def ->
      let v,e = Def.(lhs def, rhs def) in
      let v' = rename v in
      let e = substitute subs e in
      let def = Def.with_rhs (Def.with_lhs def v') e in
      Blk.Builder.add_def bld def;
      Map.add subs ~key:v ~data:v') |> fun subs ->
  Blk.Builder.result bld |>
  Term.map jmp_t ~f:(fun jmp ->
      Jmp.cond jmp |> substitute subs |> Jmp.with_cond jmp) |>
  Term.filter phi_t ~f:(fun phi ->
      Seq.length_is_bounded_by ~min:2 (Phi.defs phi))


let succ_of_jmp  jmp = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | Call t -> Option.(Call.return t >>= function
    | Direct tid -> Some tid
    | _ -> None)
  | Int (_,tid) -> Some tid
  | _ -> None

(** builds a map from a tid to all blocks that have jumps that lead
    this tid. *)
let build_rdep sub : blk term list Tid.Map.t =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:Tid.Map.empty ~f:(fun ins blk ->
      Term.to_sequence jmp_t blk |>
      Seq.fold ~init:ins ~f:(fun ins jmp ->
          Option.value_map (succ_of_jmp jmp) ~default:ins ~f:(fun tid ->
              Map.add_multi ins ~key:tid ~data:blk)))

let dominance_frontier sub entry : blk term -> blk term list =
  let module Dom = Graph.Dominator.Make(struct
      type t = (sub term * blk term list Tid.Map.t)
      module V = Blk
      let pred (_,rdep) blk = match Map.find rdep (Term.tid blk) with
        | None -> []
        | Some xs -> xs
      let succ (sub,_) blk =
        Term.to_sequence ~rev:true jmp_t blk |>
        Seq.filter_map ~f:(fun jmp -> match succ_of_jmp jmp with
            | Some t -> Term.find blk_t sub t
            | None -> None) |>
        Seq.to_list_rev
      let fold_vertex f (sub,_) init =
        Term.to_sequence blk_t sub |>
        Seq.fold ~init ~f:(fun a v -> f v a)
      let iter_vertex f (sub,_) =
        Term.to_sequence blk_t sub |> Seq.iter ~f
      let nb_vertex (sub,_) = Term.length blk_t sub
    end) in
  let cfg = sub, build_rdep sub in
  let idom = Dom.compute_idom cfg entry in
  let dom_tree = Dom.idom_to_dom_tree cfg idom in
  Dom.compute_dom_frontier cfg dom_tree idom

let iterated_frontier frontier blks =
  let df_set = Set.fold ~init:Blk.Set.empty ~f:(fun dfs b ->
      List.fold (frontier b) ~init:dfs ~f:Set.add) in
  let blks = List.fold blks ~init:Blk.Set.empty ~f:Set.add in
  let rec loop idf =
    let idf' = df_set (Set.union idf blks) in
    if Set.equal idf idf' then idf'
    else loop idf' in
  loop Blk.Set.empty

let collect_vars sub =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:Var.Set.empty ~f:(fun vars blk ->
      Term.to_sequence def_t blk |>
      Seq.fold ~init:vars ~f:(fun vars def ->
          Set.add vars (Def.lhs def)))

let blocks_that_define_var var sub =
  Term.to_sequence blk_t sub |>
  Seq.filter_map ~f:(fun blk ->
      Term.to_sequence ~rev:true def_t blk |>
      Seq.find_map ~f:(fun def ->
          Option.some_if Var.(Def.lhs def = var) (blk,def))) |>
  Seq.to_list_rev

let insert_phi x defs user =
  let put_defs init defs = List.fold defs ~init ~f:Phi.add_def in
  Term.to_sequence phi_t user |>
  Seq.find ~f:(fun phi -> Phi.lhs phi = x) |> function
  | Some phi ->
    Term.update phi_t user @@
    put_defs phi defs
  | None -> match defs with
    | [] -> user
    | d :: ds ->
      Term.append phi_t user @@
      put_defs (Phi.create x d) ds


let print_ifs_of_var frontier entry bs x =
  printf "IFS(%a)@." Var.pp x;
  Set.iter (iterated_frontier frontier (entry :: bs))
    ~f:(fun b -> printf "\t%a@." Tid.pp (Term.tid b))

let ssa_sub sub = match Term.first blk_t sub with
  | None -> sub
  | Some entry ->
    let frontier = dominance_frontier sub entry in
    collect_vars sub |> Set.fold ~init:sub ~f:(fun sub x ->
        let (bs,defs) = blocks_that_define_var x sub |>
                        List.unzip in
        iterated_frontier frontier (entry :: bs) |>
        Set.fold ~init:sub ~f:(fun sub blk ->
            match Term.find blk_t sub (Term.tid blk) with
            | None -> sub
            | Some blk ->
              Term.update blk_t sub (insert_phi x defs blk)))
    |> Term.map blk_t ~f:ssa_blk

let main' proj =
  Term.map sub_t (Project.program proj) ~f:ssa_sub |>
  printf "Program in SSA: @.%a@." Program.pp

let main proj =
  Project.with_program proj @@
  Term.map sub_t (Project.program proj) ~f:ssa_sub

let () = Project.register_pass "SSA" main
let () = Project.register_pass' "SSA'" main'
