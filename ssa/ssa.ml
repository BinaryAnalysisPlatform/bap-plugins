(** Transform to Semipruned SSA form.

    The algorithm is adopted from the following sources:

    [1]: Muchnick, Advanced Compiler Design and Implementation
         [ISBN-10: 1558603204]
    [2]: Appel, Modern Compiler Implementation in ML
         [ISBN 0-521-60764-7]
    [3]: Cooper, Engineering a Compiler, Second Edition
         [ISBN-10: 012088478X]

    Basically they describe the same algorithm but in different
    flavors and levels of detail.
*)
open Core_kernel.Std
open Bap.Std
open Format

(** extracts a successor's tid from a jump term  *)
let succ_of_jmp jmp : tid option = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | Call t -> Option.(Call.return t >>= function
    | Direct tid -> Some tid
    | _ -> None)
  | Int (_,tid) -> Some tid
  | _ -> None

(** builds a map from a tid to all blocks that have jumps that lead
    this tid. *)
let build_rdep sub : tid list Tid.Map.t =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:Tid.Map.empty ~f:(fun ins blk ->
      Term.to_sequence jmp_t blk |>
      Seq.fold ~init:ins ~f:(fun ins jmp ->
          Option.value_map (succ_of_jmp jmp) ~default:ins ~f:(fun tid ->
              Map.add_multi ins ~key:tid ~data:(Term.tid blk))))

(** Dominance relations.  *)
type dom = {
  frontier : tid -> tid list;    (** dominance frontier of a node *)
  children : tid -> tid list;    (** children of a node in a dominator tree *)
}

(** computes dominator tree an dominance frontier  *)
let create_dom rdep sub entry =
  let module Dom = Graph.Dominator.Make(struct
      type t = (sub term * tid list Tid.Map.t)
      module V = Tid
      let pred (_,rdep) tid = match Map.find rdep tid with
        | None -> []
        | Some xs -> xs
      let succ (sub,_) tid = match Term.find blk_t sub tid with
        | None -> []
        | Some blk ->
          Term.to_sequence jmp_t ~rev:true blk |>
          Seq.filter_map ~f:succ_of_jmp |>
          Seq.to_list_rev
      let fold_vertex f (sub,_) init =
        Term.to_sequence blk_t sub |>
        Seq.fold ~init ~f:(fun a v -> f (Term.tid v) a)
      let iter_vertex f (sub,_) =
        Term.to_sequence blk_t sub |>
        Seq.map ~f:Term.tid |> Seq.iter ~f
      let nb_vertex (sub,_) = Term.length blk_t sub
    end) in
  let cfg = sub, rdep in
  let idom = Dom.compute_idom cfg (Term.tid entry) in
  let dom_tree = Dom.idom_to_dom_tree cfg idom in
  {
    frontier = Dom.compute_dom_frontier cfg dom_tree idom;
    children = dom_tree
  }


(** [iterated_frontier frontier bs] given a [frontier] function, that
    for a each block [b] returns its dominance frontier, compute an
    iterated dominance frontier of a set of block [bs]. Iterated
    dominance frontier is defined inductively as
    [IDF_1(S) = DF(S); IDF_n(S) = DF(S U IDF_{n-1}(S))],
    where [DF(S)] computes a union of dominance frontiers of each
    block in [S].  The function returns a result of [IDF_k], where
    [k] is a fixpoint, i.e., such value that [IDF_k = IDF_{k-1}].  See
    section 8.11 of [1].*)
let iterated_frontier frontier blks =
  let df = Set.fold ~init:Tid.Set.empty ~f:(fun dfs b ->
      List.fold (frontier b) ~init:dfs ~f:Set.add) in
  let blks = List.fold blks ~init:Tid.Set.empty ~f:Set.add in
  let rec fixpoint idf =
    let idf' = df (Set.union idf blks) in
    if Set.equal idf idf' then idf' else fixpoint idf' in
  fixpoint Tid.Set.empty

(** [collect_vars] traverses through subroutine [sub] and collects
    variables, that are live across multiple blocks (aka globals).
    Algorithm is described in Figure 9.9 in [[3]].*)
let collect_vars sub =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:(Var.Set.empty) ~f:(fun vars blk ->
      Term.to_sequence def_t blk |>
      Seq.fold ~init:(vars,Var.Set.empty) ~f:(fun (vars,kill) def ->
          let vars =
            Exp.fold ~init:vars (object
              inherit [Var.Set.t] Bil.visitor
              method! enter_var v vars =
                if Set.mem kill v then vars
                else Set.add vars v
            end) (Def.rhs def) in
          vars,Set.add kill (Def.lhs def)) |> fst)

(** returns a list of blocks that contains [def] terms with lhs equal
    to [var] *)
let blocks_that_define_var var sub : tid list =
  Term.to_sequence blk_t sub |>
  Seq.filter ~f:(fun blk ->
      Term.to_sequence ~rev:true def_t blk |>
      Seq.exists ~f:(fun def -> Var.(Def.lhs def = var))) |>
  Seq.map ~f:Term.tid |>
  Seq.to_list_rev

(** [substitute vars exp] take a table of stacks of variables and
    for each variable in an expression [exp] perform a substitution
    of the variable to a top value of a stack for this variable, if it
    is not empty *)
let substitute vars = (object
  inherit Bil.mapper as super
  method! map_sym z =
    match Hashtbl.find vars z with
    | None | Some [] -> z
    | Some (d :: _) -> d
end)#map_exp

(** [renumber v] creates a new temporary variable with the same
    name and type as [v] *)
let renumber v = Var.(create ~tmp:true (name v) (typ v))

(** [rename dom_children phis vars sub entry] performs a renaming of
    variables in a subroutine [sub]. An algorithm is described in
    section 19.7 of [[2]] and 9.12 of [[3]] (but there is a small
    error in the latter).  The only difference is a naming scheme. The
    naming scheme is the following: we start from an original name of
    a variable, and rename of the following definitions of this
    variable with [renumber] function. It has a nice side effect of
    cleary showing a first use of a variable. And works well with our
    API to resolve input/output parameters.*)
let rename children vars sub entry =
  let vars : var list Var.Table.t = Var.Table.create () in
  let top v = match Hashtbl.find vars v with
    | None | Some [] -> v
    | Some (v :: _) -> v in
  let find_blk sub id = match Term.find blk_t sub id with
    | None -> assert false
    | Some vs -> vs in
  let new_name x =
    let y = renumber x in
    Hashtbl.add_multi vars ~key:x ~data:y;
    y in
  let rename_phis blk =
    Term.map phi_t blk ~f:(fun phi ->
        Phi.with_lhs phi (new_name (Phi.lhs phi))) in
  let rename_defs blk =
    Term.map def_t blk ~f:(fun def ->
        let rhs = Def.rhs def |> substitute vars in
        let lhs = new_name (Def.lhs def) in
        Def.with_rhs (Def.with_lhs def lhs) rhs) in
  let rename_jmps blk =
    Term.map jmp_t blk ~f:(Jmp.map_exp ~f:(substitute vars)) in
  let update_phis src dst =
    let tid = Term.tid src in
    Term.map phi_t dst ~f:(fun phi ->
        Phi.values phi |> Seq.fold ~init:phi ~f:(fun phi rhs ->
            match rhs with
            | (id,Bil.Var v) when Tid.(tid = id) ->
              Phi.update phi tid (Bil.var (top v))
            | _ -> phi)) in
  let pop_defs blk' =
    let pop v = Hashtbl.change vars v (function
        | Some (x::xs) -> Some xs
        | xs -> xs) in
    Term.to_sequence phi_t blk' |>
    Seq.iter ~f:(fun phi -> pop (Phi.lhs phi));
    Term.to_sequence def_t blk' |>
    Seq.iter ~f:(fun def -> pop (Def.lhs def)) in

  let rec rename_block sub tid =
    let blk' = find_blk sub tid in
    let blk = blk' |> rename_phis |> rename_defs |> rename_jmps in
    let sub = Term.update blk_t sub blk in
    let sub =
      Term.to_sequence jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp -> match succ_of_jmp jmp with
          | None -> sub
          | Some tid -> match Term.find blk_t sub tid with
            | None -> sub
            | Some dst ->
              Term.update blk_t sub (update_phis blk dst)) in
    let children = Tid.Set.of_list (children tid) in
    let sub = Set.fold children ~init:sub ~f:rename_block in
    pop_defs blk';
    sub in
  rename_block sub entry


let has_phi_for_var blk x =
  Term.to_sequence phi_t blk |> Seq.exists ~f:(fun phi ->
      Var.(Phi.lhs phi = x))

(** [insert_phi_node ins blk x]   *)
let insert_phi_node ins blk x =
  if has_phi_for_var blk x then blk
  else List.map ins ~f:(fun tid -> tid, Bil.var x) |>
       Phi.of_list x |>
       Term.append phi_t blk

(** [insert_phi_nodes frontier sub entry vars] given a [frontier]
    function that for a given block returns its dominance frontier, a
    subroutine [sub] with entry block [entry] and a set of variable
    [vars], insert phi node of a form [x <- phi(x,x,..,x)] for each
    variable [x] in [vars] in each block that needs it. The
    algorithm computes an iterated dominance frontier for each
    variable as per section 8.11 of [1].*)
let insert_phi_nodes frontier rdep sub entry vars =
  Set.fold vars ~init:sub ~f:(fun sub x ->
      let bs = blocks_that_define_var x sub in
      iterated_frontier frontier (entry :: bs) |>
      Set.fold ~init:sub ~f:(fun sub blk ->
          match Map.find rdep blk with
          | None | Some [] | Some [_] -> sub
          | Some ins -> match Term.find blk_t sub blk with
            | None -> assert false
            | Some blk ->
              Term.update blk_t sub (insert_phi_node ins blk x)))

(** transforms subroutine into a semi-pruned SSA form.  *)
let ssa_sub sub = match Term.first blk_t sub with
  | None -> sub
  | Some entry  ->
    let rdep = build_rdep sub in
    let dom = create_dom rdep sub entry in
    let vars = collect_vars sub in
    let sub =
      insert_phi_nodes dom.frontier rdep sub (Term.tid entry) vars in
    rename dom.children vars sub (Term.tid entry)
(* |> *)
(* Term.map blk_t ~f:(Term.filter phi_t ~f:(fun phi -> *)
(*     Seq.length_is_bounded_by ~min:2 (Phi.values phi))) *)

let ssa_program = Term.map sub_t ~f:ssa_sub

let main proj =
  printf "SSAing program\n";
  Project.with_program proj @@
  ssa_program (Project.program proj)

let () = Project.register_pass "ssa" main
