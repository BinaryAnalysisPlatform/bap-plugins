open Core_kernel.Std
open Bap.Std

let stack_top = 0x80000000L
let simpl = Exp.fixpoint Exp.fold_consts

let exp_size exp = (object
  inherit [int] Bil.visitor
  method! enter_exp _ n = n + 1
end)#visit_exp exp 0

let will_substitute = function
  | Bil.Int _ -> true
  | _ -> false

let substitute sub lhs rhs =
  Term.map blk_t sub ~f:(fun blk -> Blk.substitute blk lhs rhs)

let propagate_consts sub =
  let prop sub =
    Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
        Term.enum def_t blk |> Seq.fold ~init:sub ~f:(fun sub def ->
            let lhs = Bil.var (Def.lhs def) in
            let rhs = simpl (Def.rhs def) in
            if will_substitute rhs
            then substitute sub lhs rhs else sub)) in
  let simpl sub = Term.map blk_t sub ~f:(Blk.map_exp ~f:simpl) in
  let step sub = sub |> prop |> simpl in
  let rec fixpoint sub =
    let sub' = step sub in
    if Sub.equal sub sub' then sub else fixpoint sub' in
  fixpoint (Sub.ssa sub)

let main proj =
  let arch = Project.arch proj in
  let module Target = (val target_of_arch arch) in
  let width = Size.to_bits (Arch.addr_size arch) in
  let sp = Bil.int (Addr.of_int64 ~width stack_top) in
  let prog = Project.program proj |>
             Term.map sub_t ~f:(fun sub ->
                 propagate_consts @@
                 substitute sub Bil.(var Target.CPU.sp) sp) in

  Project.with_program proj prog


let () = Project.register_pass "fold-consts" main
