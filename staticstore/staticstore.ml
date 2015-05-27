open Core_kernel.Std
open Bap.Std
open Project
open Format

let version = "0.2"
let max_exp_size = ref 100
let yellow_format = ref "$symbol YELLOW\n"
let red_format = ref "$symbol RED\n"
let green_format = ref "$symbol GREEN\n"

let make_printer fmt sym =
  let b = Buffer.create 16 in
  Buffer.add_substitute b (function
      | "symbol" -> sym
      | s -> s) (Scanf.unescaped !fmt);
  Buffer.contents b

let yellow = make_printer yellow_format
let red = make_printer red_format
let green = make_printer green_format

module Cmdline = struct
  open Cmdliner

  let max_size : int Term.t =
    let doc = "Limit maximum depth of expression" in
    Arg.(value & opt int !max_exp_size & info ["max-exp-size"] ~doc)

  let print : [`green | `yellow | `red] list Term.t =
    let variants = [
      "green", `green;
      "yellow", `yellow;
      "red", `red
    ] in
    let doc = sprintf
        "Print result. Accepted values are %s. Multiple variants \
         can be specified by enumerating this option several times" @@
      Arg.doc_alts_enum variants in
    Arg.(value & opt_all (enum variants) [] & info ["print"] ~doc)

  let format name default : string Term.t =
    let doc = sprintf "Print %s using specified format. \
                       Every occurence of $symbol is substituted with\
                       a symbol name" name in
    Arg.(value & opt string default &
         info [sprintf "%s-format" name] ~doc)

  let info =
    let doc = "classify all functions based on memory store \
               operations. If address of a store operation \
               is known, then it would be classified as safe \
               or green." in
    let man = [
      `S "DESCRIPTION";
      `P "This plugin will classify all functions into three categories:";
      `Noblank;
      `P "- red;"; `Noblank;
      `P "- yellow;"; `Noblank;
      `P "- green.";
      `P
        "`green` functions perform all writes to memory \
         only to a statically known offsets, i.e., a compile \
         time constants. SP is also considered a constant \
         iff it is only defined with constant in the ENTRY or EXIT \
         blocks. If it is defined by a non-constant value, like \
         `SP := SP - R0` it is considered unsafe. If it is defined \
         by a constant value, but outside of the ENTRY or EXIT \
         blocks, but the overall function is classified as green, \
         then such function will be classified as `yellow`.:"
    ] in
    Term.info ~man ~doc "staticstore" ~version

  let options size green yellow red printers =
    max_exp_size := size;
    List.iter [green_format; yellow_format; red_format] ~f:(fun fmt ->
        fmt := "");
    List.iter printers ~f:(function
        | `green -> green_format := green;
        | `yellow -> yellow_format := yellow;
        | `red -> red_format := red)

  let main =
    Term.(pure options $max_size
          $format "green" !green_format
          $format "yellow" !yellow_format
          $format "red" !red_format
          $print)

  let eval argv =
    match Term.eval ~argv (main,info) with
    | `Ok () -> ()
    | _ -> exit 1

end

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

  let optimize : bil -> bil =
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
    if exp_size exp < !max_exp_size then var,exp
    else
      let regs = (object
        inherit [String.Set.t] Bil.visitor
        method! enter_var v s = Set.add s (Var.to_string v)
      end)#visit_exp exp String.Set.empty in
      let desc = String.concat ~sep:", " @@
        Set.elements regs in
      var, Bil.unknown desc reg32_t

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
      if Exp.(exp = exp') then exp
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

let stub_names = [".plt"; "__symbol_stub"; "__picsymbol_stub"]

let main argv project =
  Cmdline.eval argv;
  let module Target = (val target_of_arch project.arch) in
  let module Main = Main(Target) in
  let blocks = Disasm.blocks project.disasm in
  let is_plt mem =
    Memmap.dominators project.memory mem |>
    Seq.exists ~f:(fun (_,tag) -> match Value.get Image.region tag with
        | Some name -> List.mem stub_names name
        | None -> false) in
  let annotate bound sym annots : value memmap =
    match Table.find_addr blocks (Memory.min_addr bound) with
    | None -> annots
    | Some (_,entry) -> match Main.collect_unsafe ~bound entry with
      | [] when Main.modifies_sp_in_the_middle ~bound entry ->
        print_string (yellow sym);
        Memmap.add annots bound (Value.create color `yellow)
      | [] ->
        print_string (green sym);
        Memmap.add annots bound (Value.create color `green)
      | _  ->
        print_string (red sym);
        Memmap.add annots bound (Value.create color `red) in
  let memory = Table.foldi project.symbols ~init:project.memory
      ~f:(fun mem sym annots ->
          if is_plt mem then annots
          else annotate mem sym annots) in
  {project with memory}

let () = register_plugin_with_args main
