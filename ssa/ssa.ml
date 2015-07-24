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

module Cfg = Graphlib.Ir

(** [iterated_frontier frontier bs] given a [frontier] function, that
    for a each block [b] returns its dominance frontier, compute an
    iterated dominance frontier of a set of block [bs]. Iterated
    dominance frontier is defined inductively as
    [IDF_1(S) = DF(S); IDF_n(S) = DF(S U IDF_{n-1}(S))],
    where [DF(S)] computes a union of dominance frontiers of each
    block in [S].  The function returns a result of [IDF_k], where
    [k] is a fixpoint, i.e., such value that [IDF_k = IDF_{k-1}].  See
    section 8.11 of [1].*)
let iterated_frontier f blks =
  let df = Set.fold ~init:Blk.Set.empty ~f:(fun dfs b ->
      Seq.fold (Frontier.enum f b) ~init:dfs ~f:Set.add) in
  let blks = List.fold blks ~init:Blk.Set.empty ~f:Set.add in
  let rec fixpoint idf =
    let idf' = df (Set.union idf blks) in
    if Set.equal idf idf' then idf' else fixpoint idf' in
  fixpoint Blk.Set.empty

let vars_of_exp = Exp.fold ~init:Var.Set.empty (object
    inherit [Var.Set.t] Bil.visitor
    method! enter_var var vars = Set.add vars var
  end)

let vars_of_label = function
  | Indirect exp -> vars_of_exp exp
  | Direct _ -> Var.Set.empty

(** [collect_vars] traverses through subroutine [sub] and collects
    variables, that are live across multiple blocks (aka globals).
    Algorithm is described in Figure 9.9 in [[3]].*)
let collect_vars sub =
  let (--) = Set.diff and (++) = Set.union in
  Term.enum blk_t sub |>
  Seq.fold ~init:Var.Set.empty ~f:(fun vars blk ->
      Term.enum def_t blk |>
      Seq.fold ~init:(vars,Var.Set.empty) ~f:(fun (vars,kill) def ->
          vars ++ (vars_of_exp (Def.rhs def) -- kill),
          Set.add kill (Def.lhs def)) |> fun (vars,kill) ->
      Seq.fold ~init:Var.Set.empty (Term.enum jmp_t blk)
        ~f:(fun vars jmp ->
            vars ++ vars_of_exp (Jmp.cond jmp) ++
            match Jmp.kind jmp with
            | Ret dst | Goto dst -> vars_of_label dst
            | Int (_,_) -> Var.Set.empty
            | Call call ->
              vars_of_label (Call.target call) ++
              match Call.return call with
              | None -> Var.Set.empty
              | Some dst -> vars_of_label dst) -- kill ++ vars)



(** returns a list of blocks that contains [def] terms with lhs equal
    to [var] *)
let blocks_that_define_var var sub : blk term list =
  Cfg.nodes sub |>
  Seq.filter ~f:(fun blk ->
      Term.enum ~rev:true def_t blk |>
      Seq.exists ~f:(fun def -> Var.(Def.lhs def = var))) |>
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

(** [rename dom_children phis vars sub entry] performs a renaming of
    variables in a subroutine [sub]. An algorithm is described in
    section 19.7 of [[2]] and 9.12 of [[3]] (but there is a small
    error in the latter).  The only difference is a naming scheme. The
    naming scheme is the following: we start from an original name of
    a variable, and rename of the following definitions of this
    variable with [renumber] function. It has a nice side effect of
    cleary showing a first use of a variable. And works well with our
    API to resolve input/output parameters.*)
let rename dom vars sub entry =
  let vars : var list Var.Table.t = Var.Table.create () in
  let nums : int Var.Table.t = Var.Table.create () in
  let top v = match Hashtbl.find vars v with
    | None | Some [] -> v
    | Some (v :: _) -> v in
  let new_name x =
    Hashtbl.change nums x (function
        | None -> Some 1
        | Some x -> Some (x + 1));
    let n = Hashtbl.find_exn nums x in
    let y = Var.renumber x n in
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
    Term.enum phi_t blk' |>
    Seq.iter ~f:(fun phi -> pop (Phi.lhs phi));
    Term.enum def_t blk' |>
    Seq.iter ~f:(fun def -> pop (Def.lhs def)) in

  let rec rename_block sub blk' =
    let blk = blk' |> rename_phis |> rename_defs |> rename_jmps in
    let sub = Cfg.Node.update blk sub in
    let sub =
      Cfg.Node.succs blk sub |> Seq.fold ~init:sub ~f:(fun sub dst ->
          Cfg.Node.update (update_phis blk dst) sub) in
    let children = Cfg.nodes sub |>
                   Seq.filter ~f:(Tree.is_child_of ~parent:blk dom) in
    let sub = Seq.fold children ~init:sub ~f:rename_block in
    pop_defs blk;
    sub in
  rename_block sub entry

let has_phi_for_var blk x =
  Term.enum phi_t blk |> Seq.exists ~f:(fun phi -> Var.(Phi.lhs phi = x))

(** [insert_phi_node ins blk x]   *)
let insert_phi_node ins blk x =
  if has_phi_for_var blk x then blk
  else Seq.map ins ~f:(fun blk -> Term.tid blk, Bil.var x) |>
       Seq.to_list_rev |> Phi.of_list x |>
       Term.append phi_t blk

(** [insert_phi_nodes frontier sub entry vars] given a [frontier]
    function that for a given block returns its dominance frontier, a
    subroutine [sub] with entry block [entry] and a set of variable
    [vars], insert phi node of a form [x <- phi(x,x,..,x)] for each
    variable [x] in [vars] in each block that needs it. The
    algorithm computes an iterated dominance frontier for each
    variable as per section 8.11 of [1].*)
let insert_phi_nodes frontier sub entry vars =
  Set.fold vars ~init:sub ~f:(fun sub x ->
      let bs = blocks_that_define_var x sub in
      iterated_frontier frontier (entry :: bs) |>
      Set.fold ~init:sub ~f:(fun sub blk ->
          let blk = Term.find_exn blk_t (Cfg.to_sub sub) (Term.tid blk) in
          let ins = Cfg.Node.preds blk sub in
          Cfg.Node.update (insert_phi_node ins blk x) sub))

(** transforms subroutine into a semi-pruned SSA form.  *)
let ssa_sub sub =
  match Term.first blk_t sub with
  | None -> sub
  | Some entry ->
    let vars = collect_vars sub in
    let cfg = Cfg.of_sub sub in
    let dom = Graphlib.dominators (module Cfg) cfg entry in
    let dom_frontier = Graphlib.dom_frontier (module Cfg) cfg dom in
    let sub = insert_phi_nodes dom_frontier cfg entry vars in
    rename dom vars sub entry |>
    Cfg.to_sub

let ssa_program = Term.map sub_t ~f:ssa_sub

let main proj =
  Project.with_program proj @@
  ssa_program (Project.program proj)

let () = Project.register_pass "ssa" main
