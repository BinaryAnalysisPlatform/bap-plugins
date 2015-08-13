open Core_kernel.Std
open Bap.Std
open Parameters

let stack_offset = 0x40000000L

let simpl_load lookup =
  Exp.map (object(self)
    inherit Bil.mapper as super
    method! map_load ~mem ~addr endian size =
      let default = super#map_load ~mem ~addr endian size in
      match mem with
      | Bil.Var mem -> Option.value (lookup mem addr) ~default
      | exp -> self#map_exp exp
  end)

let simpl lookup =
  Exp.fixpoint (Fn.compose (simpl_load lookup) Exp.fold_consts)

let will_substitute = function
  | Bil.Int _ -> true
  | _ -> false

let substitute sub lhs rhs =
  Term.map blk_t sub ~f:(fun blk -> Blk.substitute blk lhs rhs)

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
  | Some (Some addr) ->
    eprintf "Making address from string:\n%!";
    Some (make_addr (Int64.of_string addr))
  | Some None ->
    match Memmap.max_binding (Project.memory proj) with
    | None -> Some (make_addr stack_offset)
    | Some (mem,_) ->
      let max_addr = Memory.max_addr mem in
      Some Addr.(max_addr + make_addr stack_offset)

let run proj =
  let arch = Project.arch proj in
  let module Target = (val target_of_arch arch) in
  let width = Size.to_bits (Arch.addr_size arch) in
  let substitute_sp =
    let make_addr x = Addr.of_int64 ~width x in
    match get_sp_base make_addr proj with
    | None -> ident
    | Some sp -> fun sub ->
      substitute sub Bil.(var Target.CPU.sp) (Bil.Int sp) in
  let is_mem = Target.CPU.is_mem in
  let prog = Project.program proj |>
             Term.map sub_t ~f:(fun sub ->
                 let mem = Memdep.create is_mem sub in
                 let lookup = Memdep.lookup mem in
                 let simpl = simpl lookup in
                 propagate_consts simpl @@
                 substitute_sp sub) in
  Project.with_program proj prog
