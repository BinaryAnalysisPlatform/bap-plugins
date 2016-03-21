(* Dead code elimination based on SSA form.

   If a program is in a SSA form, then dead code elimination is
   trivial. All variables, that are not used, are dead. Sic!

   We perform this optimization in a two passes: first we collect use
   and def sets, and afterwards filter out all variables that are
   defined, but not used.
*)


open Core_kernel.Std
open Bap.Std

(* note, this function will return all variables, including
   non-free *)
let vars_of_exp = Exp.fold ~init:Var.Set.empty (object
    inherit [Var.Set.t] Exp.visitor
    method! enter_var var vars = Set.add vars var
  end)

let vars_of_label = function
  | Indirect exp -> vars_of_exp exp
  | Direct _ -> Var.Set.empty

let collect_vars sub =
  let (++) = Set.union in
  Term.enum blk_t sub |>
  Seq.fold ~init:(Var.Set.empty,Var.Set.empty) ~f:(fun sets blk ->
      Blk.elts blk |> Seq.fold ~init:sets ~f:(fun (defs,uses) ->
          function
          | `Phi phi ->
            Set.add defs (Phi.lhs phi),
            Seq.fold (Phi.values phi) ~init:uses ~f:(fun uses (_,exp) ->
                uses ++ vars_of_exp exp)
          | `Def def ->
            Set.add defs (Def.lhs def),
            uses ++ vars_of_exp (Def.rhs def)
          | `Jmp jmp ->
            defs,
            uses ++ vars_of_exp (Jmp.cond jmp) ++
            match Jmp.kind jmp with
            | Ret dst | Goto dst -> vars_of_label dst
            | Int (_,_) -> Var.Set.empty
            | Call call ->
              uses ++ vars_of_label (Call.target call) ++
              match Call.return call with
              | None -> Var.Set.empty
              | Some dst -> vars_of_label dst))

let clean_sub arch sub =
  let module Target = (val target_of_arch arch) in
  let no_side_effects var =
    let open Target.CPU in
    Var.is_virtual var || is_flag var in

  let filter dead t lhs blk =
    Term.filter t blk ~f:(fun p -> not(Set.mem dead (lhs p))) in
  let rec clean sub =
    let defs,uses = collect_vars sub in
    let dead = Set.diff defs uses |> Set.filter ~f:no_side_effects in
    if Set.is_empty dead then sub
    else Term.map blk_t sub ~f:(fun blk ->
        blk |>
        filter dead phi_t Phi.lhs |>
        filter dead def_t Def.lhs) |> clean  in
  clean sub

let main proj =
  Project.program proj |>
  Term.map sub_t ~f:(clean_sub (Project.arch proj)) |>
  Project.with_program proj

let () = Project.register_pass ~deps:["ssa"] main
