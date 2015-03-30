open Core_kernel.Std
open Bap.Std
open Program_visitor
open Format

let max_exp_size = 100

module Main(Env : sig
    val bound  : mem
    val annots : (string * string) memmap
    module Target : Target
  end) = struct
  open Env
  open Target.CPU

  let defines_stack var = is_sp var || is_bp var

  (** Substitute PC with its value  *)
  let resolve_pc mem = Bil.map (object
      inherit Bil.mapper as super
      method! map_var var =
        if Target.CPU.is_pc var then
          Bil.int (Target.CPU.addr_of_pc mem)
        else super#map_var var
    end)

  let optimize =
    List.map ~f:Bil.fixpoint [
      Bil.fold_consts;
      Bil.prune_unreferenced;
    ] |> List.reduce_exn ~f:Fn.compose |> Bil.fixpoint

  let bil_of_block blk =
    Block.insns blk
    |> List.concat_map ~f:(fun (mem,insn) ->
        (resolve_pc mem (Insn.bil insn)))
    |> optimize

  let simpl exp =
    match optimize [Stmt.jmp exp] with
    | [Stmt.Jmp r] -> r
    | _ -> assert false

  type subst = exp Var.Map.t

  let exp_size exp = (object
    inherit [int] Bil.visitor
    method! enter_exp _ n = n + 1
  end)#visit_exp exp 0

  let squash_complex_exp exp =
    if exp_size exp < max_exp_size then exp
    else
      let regs = (object
        inherit [String.Set.t] Bil.visitor
        method! enter_var v s = Set.add s (Var.to_string v)
      end)#visit_exp exp String.Set.empty in
      let desc = String.concat ~sep:", " @@
        Set.elements regs in
      Exp.unknown desc reg32_t

  let subst_exp u ~in_ ~to_:x = (object
    inherit Bil.mapper
    method! map_var v =
      if Var.(v = u) then x else Bil.var v
  end)#map_exp in_

  (* this simplification assumes that store/load folding step
     was performed earlier *)
  let simpl_memory =
    (object(self)
      inherit Bil.mapper as super
      method! map_exp = function
        | Bil.Load (_,addr,e,s) ->
          Bil.Load (Bil.var mem, self#map_exp addr,e,s)
        | Bil.Store (m,idx, Bil.Load(_,idx',e',s'),e,s) ->
          Bil.Store (self#map_exp m,
                     self#map_exp idx,
                     Bil.Load(
                       Bil.var mem,
                       self#map_exp idx',e',s'),e,s)
        | exp -> super#map_exp exp
    end)#map_exp

  let simpl_memory exp =
    let rec loop exp =
      let exp' = simpl_memory exp in
      if Bil.equal exp exp' then exp
      else loop exp' in
    loop exp

  let apply_subst subst exp =
    Map.fold subst ~init:exp ~f:(fun ~key ~data exp ->
        subst_exp key ~in_:exp ~to_:data)

  let propogate init bil =
    let (sps,esp) = Bil.fold ~init:([], init)
        (object
          inherit [(stmt * subst) list * subst] Bil.visitor
          method! leave_move v exp (stmts, subs) =
            let exp = apply_subst subs exp in
            stmts, Map.add ~key:v ~data:exp subs |>
                   Map.map ~f:simpl_memory |>
                   Map.map ~f:squash_complex_exp
          method! enter_stmt stmt (stmts,subs) =
            (stmt,subs) :: stmts, subs
        end) bil in
    List.rev_map sps ~f:(fun (stmt,subst) ->
        Map.fold ~init:[stmt] subst ~f:(fun ~key ~data stmt ->
            Bil.substitute_var key data stmt))
    |> List.concat |> optimize, esp

  let is_safe_index exp = (object
    inherit [bool] Bil.visitor
    method! enter_load ~mem:_ ~addr _ _ r =
      r && match addr with
      | Bil.Int _ -> true
      | Bil.BinOp (_,Bil.Var var, Bil.Int _) ->
        defines_stack var
      | _ -> false
    method! enter_special _ _ = false
    method! enter_var var r = r && defines_stack var
  end)#visit_exp exp true

  let collect_unsafe blk =
    let bil_of_first_blk,subst =
      let bil,subst = bil_of_block blk |>
                      propogate Var.Map.empty in
      optimize bil,subst in
    let subst = Map.filter subst ~f:(fun ~key ~data:_ ->
        defines_stack key) in
    let bil_of_block blk' =
      if Block.(blk = blk') then bil_of_first_blk else
        bil_of_block blk' |> propogate subst |> fst |> optimize in
    Block.dfs ~bound blk |>
    Seq.fold ~init:[] ~f:(fun unsafe blk ->
        Bil.fold ~init:unsafe
          (object
            inherit [exp list] Bil.visitor
            method! enter_store ~mem:_ ~addr ~exp:_ _ _ unsafe =
              if is_safe_index addr then unsafe
              else addr :: unsafe
          end) (bil_of_block blk))

  let modifies_sp_in_the_middle entry =
    let exits =
      Block.dfs ~bound entry |> Seq.filter ~f:(fun blk ->
          Seq.length_is_bounded_by ~max:1 (Block.dfs ~bound blk)) |>
      Seq.to_list_rev |> Block.Set.of_list in
    Block.dfs ~bound entry |>
    Seq.exists ~f:(fun blk ->
        not Block.(blk = entry || Set.mem exits blk) &&
        Bil.find (object
          inherit [unit] Bil.finder
          method move var _ find =
            if defines_stack var then
              find.return (Some ());
            find
        end) (bil_of_block blk))
end

let main project =
  let module Target = (val target_of_arch project.arch) in

  let blocks = Disasm.blocks project.program in
  let plt = Memmap.to_sequence project.annots |>
            Seq.find ~f:(fun (_,(tag,name)) ->
                tag = "section" && name = ".plt") |> uw |> fst in
  let print mem sym =
    let module Main = Main(struct
        let bound = mem
        let annots = project.annots
        module Target = Target
      end) in
    let _,entry =
      uw (Table.find_addr blocks (Memory.min_addr mem)) in
    match Main.collect_unsafe entry with
    | [] when Main.modifies_sp_in_the_middle entry ->
      printf "%s YELLOW@.%!" sym
    | [] -> printf "%s GREEN@.%!" sym
    | _ -> printf "%s RED@.%!" sym in
  Table.iteri project.symbols ~f:(fun mem sym ->
      if not (Memory.contains plt (Memory.min_addr mem))
      then print mem sym)

let () = register' main
