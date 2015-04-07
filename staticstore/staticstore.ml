open Core_kernel.Std
open Bap.Std
open Program_visitor
open Format

let max_exp_size = 100

module Main(Target : Target) = struct
  open Target.CPU

  let defines_stack var = is_sp var || is_bp var

  (** Substitute PC with its value  *)
  let resolve_pc mem =
    Bil.map (object
      inherit Bil.mapper as super
      method! map_var var =
        if Target.CPU.is_pc var then
          Bil.int (Target.CPU.addr_of_pc mem)
        else super#map_var var
    end)

  let optimize =
    List.map ~f:Bil.fixpoint [
      Bil.fold_consts;
    ] |> List.reduce_exn ~f:Fn.compose |> Bil.fixpoint

  let bil_of_block blk =
    Block.insns blk
    |> List.concat_map ~f:(fun (mem,insn) ->
        (resolve_pc mem (Insn.bil insn)))
    |> optimize

  type subst = (var * exp) list

  let exp_size exp = (object
    inherit [int] Bil.visitor
    method! enter_exp _ n = n + 1
  end)#visit_exp exp 0

  let squash_complex_exp (var,exp) =
    if exp_size exp < max_exp_size then var,exp
    else
      let regs = (object
        inherit [String.Set.t] Bil.visitor
        method! enter_var v s = Set.add s (Var.to_string v)
      end)#visit_exp exp String.Set.empty in
      let desc = String.concat ~sep:", " @@
        Set.elements regs in
      var,Exp.unknown desc reg32_t

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

  let simpl_memory (var,exp) =
    let rec loop exp =
      let exp' = simpl_memory exp in
      if Bil.equal exp exp' then exp
      else loop exp' in
    var,loop exp

  let apply_subst subst exp =
    List.fold subst ~init:exp ~f:(fun exp (lhs,rhs) ->
        subst_exp lhs ~in_:exp ~to_:rhs)

  let use_or_def u (v,x) =
    Var.(v = u) || (object
      inherit [bool] Bil.visitor
      method! enter_var v found = found || Var.(v = u)
    end)#visit_exp x false
  let doesn't_use_or_def u = Fn.non (use_or_def u)

  let propogate init bil =
    let (sps,esp) = List.fold ~init:([], init)
        ~f:(fun (stmts,subst) stmt ->
            let subst' =
              Bil.fold ~init:subst (object
                inherit [subst] Bil.visitor
                method! leave_move v exp subs =
                  let exp = apply_subst subs exp in
                  (v,exp) :: List.filter ~f:(doesn't_use_or_def v) subs |>
                  List.map ~f:simpl_memory |>
                  List.map ~f:squash_complex_exp
              end) [stmt] in
            (stmt,subst) :: stmts, subst') bil in
    List.rev_map sps ~f:(fun (stmt,subst) ->
        Bil.map (object
          inherit Bil.mapper
          method! map_var v =
            match List.find subst ~f:(fun (lhs,_) -> Var.(v = lhs)) with
            | Some (_,rhs) -> rhs
            | _ -> Bil.var v
        end) [stmt]) |> List.concat,esp

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

  let collect_unsafe ~bound blk =
    let bil_of_first_blk,subst =
      let bil,subst = bil_of_block blk |>
                      propogate [] in
      optimize bil,subst in
    let subst = List.filter subst ~f:(fun (var,_) ->
        defines_stack var) in
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

  let modifies_sp_in_the_middle ~bound entry =
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
  let module Main = Main(Target) in
  let blocks = Disasm.blocks project.program in
  let is_plt = Memmap.to_sequence project.annots |>
               Seq.find ~f:(fun (_,(tag,name)) ->
                   tag = "section" && name = ".plt") |> function
               | None -> fun _ -> false
               | Some (mem,_) -> fun sym -> 
                 Memory.contains mem (Memory.min_addr sym) in
  let annotate bound _sym annots =
    match Table.find_addr blocks (Memory.min_addr bound) with 
    | None -> annots
    | Some (_,entry) -> match Main.collect_unsafe ~bound entry with
    | [] when Main.modifies_sp_in_the_middle ~bound entry ->
      Memmap.add annots bound ("staticstore", "yellow")
    | [] -> Memmap.add annots bound ("staticstore", "green")
    | _  -> Memmap.add annots bound ("staticstore", "red") in
  let annots = Table.foldi project.symbols ~init:project.annots
      ~f:(fun mem sym annots -> 
          if is_plt mem then annots
          else annotate mem sym annots) in
  {project with annots}


let () = register main

(* 
__mh_execute_header            0x100000000:64 3792
_main                          0x100000ED0:64 134 
sub_100000f56                  0x100000F56:64 6   
sub_100000f5c                  0x100000F5C:64 6   
sub_100000f62                  0x100000F62:64 6   



 *)
