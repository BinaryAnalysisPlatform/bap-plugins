open Core_kernel.Std
open Bap.Std
open Parameters
open Utils

let stack_offset = 0x40000000L

let simpl_load lookup =
  Exp.map (object(self)
    inherit Bil.mapper as super
    method! map_load ~mem ~addr endian size =
      match lookup ~mem ~addr endian size with
      | None -> super#map_load ~mem ~addr endian size
      | Some exp -> exp
  end)

let simpl lookup =
  Exp.fixpoint (Fn.compose (simpl_load lookup) Exp.fold_consts)

let will_substitute = function
  | Bil.Int _ -> true
  | _ -> false

let substitute sub lhs rhs =
  Term.map blk_t sub ~f:(fun blk -> Blk.substitute blk lhs rhs)

let clobbers blk =
  Term.enum jmp_t blk |> Seq.filter_map ~f:(fun jmp ->
      match Jmp.kind jmp with
      | Goto _ | Ret _ -> None
      | Int (_,tid) -> Some tid
      | Call call -> match Call.return call with
        | Some (Direct tid) -> Some tid
        | _ -> None)

let clobber_blk globs arch blk =
  let module Target = (val target_of_arch arch) in
  Set.fold (Set.inter Target.CPU.gpr globs) ~init:blk ~f:(fun blk var ->
      let typ = Var.typ var in
      Def.create var Bil.(unknown "clobberred_by_call" typ) |>
      Term.prepend def_t blk)

let globals sub =
  Term.enum blk_t sub |> Seq.fold ~init:Var.Set.empty ~f:(fun vs blk ->
      Set.union vs (Blk.free_vars blk))

let clobber_sub arch sub =
  if Sub.is_ssa sub then
    invalid_arg "Input must be in non SSA form";
  let module Target = (val target_of_arch arch) in
  let addr_t = Type.Imm (Size.in_bits (Arch.addr_size arch)) in
  let data_t = Type.Imm (Size.in_bits (mem_elt_size arch)) in
  let undef_mem =
    let exp = Bil.unknown "clobbered_by_call" data_t in
    let en = Arch.endian arch in
    let addr = Bil.(unknown "any_addr" addr_t) in
    let lhs = Target.CPU.mem in
    let mem = Bil.var lhs in
    let rhs = Bil.store ~mem ~addr exp en (mem_elt_size arch) in
    Def.create lhs rhs in
  let clobbered =
    Term.enum blk_t sub |>
    Seq.fold ~init:Tid.Set.empty ~f:(fun set blk ->
        Seq.fold (clobbers blk) ~init:set ~f:Set.add) in
  let globals = globals sub in
  Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      if Set.mem clobbered (Term.tid blk) then
        let blk = Term.prepend def_t blk undef_mem in
        Term.update blk_t sub (clobber_blk globals arch blk)
      else sub)

let propagate_consts simpl sub =
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

let get_sp_base make_addr proj =
  match options.fix_sp with
  | None -> None
  | Some (Some addr) -> Some (make_addr (Int64.of_string addr))
  | Some None ->
    match Memmap.max_binding (Project.memory proj) with
    | None -> Some (make_addr stack_offset)
    | Some (mem,_) ->
      let max_addr = Memory.max_addr mem in
      Some Addr.(max_addr + make_addr stack_offset)

let run proj =
  let arch = Project.arch proj in
  let module Target = (val target_of_arch arch) in
  let width = Size.in_bits (Arch.addr_size arch) in
  let substitute_sp =
    let make_addr x = Addr.of_int64 ~width x in
    match get_sp_base make_addr proj with
    | None -> ident
    | Some sp -> fun sub ->
      substitute sub Bil.(var Target.CPU.sp) (Bil.Int sp) in
  let prog = Project.program proj |>
             Term.map sub_t ~f:(fun sub ->
                 let sub = Sub.ssa (clobber_sub arch sub) in
                 let memory = match options.resolve_loads with
                   | `no -> None
                   | `ro | `rw -> Some (Project.memory proj) in
                 let mem = Memdep.create ?memory arch sub in
                 let lookup = Memdep.load mem in
                 let simpl = simpl lookup in
                 propagate_consts simpl @@
                 substitute_sp sub) in
  Project.with_program proj prog
