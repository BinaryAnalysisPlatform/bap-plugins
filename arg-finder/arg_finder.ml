open Core_kernel.Std
open Bap.Std
open Option
open Format

type arg_result_container = {sym : string;
                             arg_addr : Addr.t;
                             min_function_addr : Addr.t;
                             max_function_addr : Addr.t}

module Cmdline = struct
  open Cmdliner

  let info =
    let doc =
      "Output addresses for function arguments" in
    Term.info ~doc "Argument finder"

  let outfile : string Term.t =
    let doc = "New line separated list of addresses
      of arguments to functions" in
    Arg.(value & opt string "" & info ["outfile"] ~doc)

  let sym_outfile : string Term.t =
    let doc = "New line separated list of functions and their
      boundaries for which arguments were found" in
    Arg.(value & opt string "" & info ["sym-outfile"] ~doc)

  let verbose : bool Term.t =
    let doc = "Turn on debugging" in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)

  let process_args outfile sym_outfile verbose =
    outfile,sym_outfile,verbose

  let parse argv =
    Term.eval ~argv
      (Term.(pure process_args $outfile $sym_outfile $verbose),
       info) |>
    function
    | `Ok x -> x
    | _ -> exit 1
end

module Main(Target : Target) = struct
  open Target.CPU

  let strip = String.filter ~f:(fun x -> x <> '\n')

  let write_output output outfile =
    let filename = outfile in
    Out_channel.with_file filename ~f:(fun chan ->
        Out_channel.output_lines chan (output |> List.map ~f:(fun x ->
            sprintf "%x" (Word.to_int x |> ok_exn))))

  let write_output_syms syms_and_bounds outfile =
    let lines =
      List.map syms_and_bounds ~f:(fun x ->
          let sym,min,max = x in
          sprintf "%s %x %x" sym min max) in
    let filename = "sym_"^outfile in
    Out_channel.with_file filename ~f:(fun chan ->
        Out_channel.output_lines chan lines)

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
    List.map ~f:Bil.fixpoint [Bil.fold_consts]
    |> List.reduce_exn ~f:Fn.compose |> Bil.fixpoint

  (** substitute loads with the value of corresponding memory *)
  let resolve_indirects project =
    Bil.map (object inherit Bil.mapper as super
      method! map_load ~mem ~addr endian scale =
        let exp = super#map_load ~mem ~addr endian scale in
        match addr with
        | Bil.Int addr ->
          let exp = Memmap.lookup (Project.memory project) addr |> Seq.hd |> function
            | None -> exp
            | Some (mem,_) -> match Memory.get ~scale ~addr mem with
              | Ok w -> Bil.int w
              | _ -> exp in
          exp
        | _ -> exp
    end)

  (* Retrieve the statements and associated memory which populate function call
   * arguments *)
  let get_stmts block exps project =
    (* get bil stmts and associated memory *)
    let number_of_args = List.length exps in
    let stmts =
      Block.insns block |> List.map ~f:(fun (mem,insn) ->
          List.cartesian_product [mem] (Insn.bil insn)) |>
      List.concat in
    List.fold stmts ~init:[] ~f:(fun acc (mem,stmt) ->
        let stmt = resolve_pc mem [stmt] |> optimize |>
                   resolve_indirects project |> List.hd_exn in
        match stmt with
        | Bil.Move (var,expr) ->
          (* up cast the lhs to an expression type so that we can compare it *)
          let lhs_as_exp = Bil.var var in
          (* If
            * 1) this Move's lhs matches one of the register names in exps
           * (statements of our args), and
           * 2) it is not already in our accumulator, and
           * 3) Our accumulator does not already contain the maximum number of
           * args that we expect for this function then
           * add it *)
          if List.exists exps ~f:(fun e -> e = lhs_as_exp) &&
             (not @@ List.exists acc ~f:(fun (_mem,e) -> e = lhs_as_exp)) &&
             List.length acc < number_of_args then
            (mem,expr) :: acc else acc
        | _ -> acc)

  let min x =
    Memory.min_addr x

  let output_rodata project exp ro_section_mem =
    match exp with
    | Bil.Int addr ->
        if Memory.contains ro_section_mem addr then
          Format.printf "  [.rodata]"
    | _ -> ()

  (* Gets the ABI information of a [dest_block]. The custom ABI module
   * only returns ABI information here if it's one of the funtions we
   * are interested in, e.g. __strcpy_chk *)
  let handle_dest block dest_block project parent_sym acc verbose ro_section_mem =
    let symbols = Project.symbols project in
    let res = Symtab.find_by_start symbols @@ Block.addr dest_block in
    Option.fold ~init: acc res ~f:(fun acc fn ->
        (* if the target matches strcpy, etc., we want to find its args *)
        let sym_name = Symtab.name_of_fn fn in
        let abi = new Custom_arm_abi.custom ~sym:sym_name mem dest_block in
        let args = abi#args in
        if args <> [] then
          begin
            if verbose then
              Format.printf "\nFunc %s\n" sym_name;
            let extract_args = List.map args ~f:snd in
            let result = get_stmts block extract_args project in
            let find_bound f =
              Symtab.memory_of_fn symbols fn |> f
              |> Option.value ~default:(Word.of_int ~width:32 0) in
            let max_mem_of_fn = find_bound Memmap.max_addr in
            let min_mem_of_fn = find_bound Memmap.min_addr in
            List.foldi ~init:acc result ~f:(fun i acc (mem,exp) ->
                if verbose then begin
                  let hex = Addr.to_int (min mem) |> ok_exn in
                  Format.printf "Arg %d at %x" i hex;
                  output_rodata project exp ro_section_mem;
                  print_newline ();
                end;
                let disasm = Project.disasm project in
                let st = Disasm.insn_at_addr disasm (min mem) in
                match st with
                | Some (mem,insn) ->
                  (* symbol, address of the arg of interest,
                     min and max addr of this function *)
                  let res = {sym = parent_sym;
                             arg_addr = min mem;
                             min_function_addr = min_mem_of_fn;
                             max_function_addr = max_mem_of_fn} in
                  res :: acc
                | None -> acc)
          end
        else acc)

  let find_section_by_name memory name =
    Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
        Option.(Value.get Image.section x >>= fun n ->
                Option.some_if (n = name) m))

  (* Gets blocks which are associated with calls and passes
   * these on to attempt retrieval of args *)
  let analyze sym bound entry project verbose =
    let ro_section_mem = match find_section_by_name (Project.memory project) ".rodata" with
      | Some mem -> mem
      | None -> printf "no rodata"; exit 1 in
    let blocks = Block.dfs ~bound entry in
    Seq.fold ~init:[] blocks ~f:(fun acc block ->
        Seq.fold ~init:acc (Block.dests block) ~f:(fun acc dests ->
            match dests with
            | `Unresolved _ -> acc
            | `Block (_, `Cond) -> acc
            | `Block (_, `Fall) -> acc
            | `Block (dest_block, `Jump) ->
              handle_dest block dest_block project sym acc verbose ro_section_mem))
end

let main args project =
  let arch = Project.arch project in
  let module Target = (val target_of_arch arch) in
  let module Main = Main(Target) in
  let outfile,sym_outfile,verbose = Cmdline.parse args in
  let syms = Project.symbols project |> Symtab.to_sequence in
  let result =
    Seq.fold ~init:[] syms ~f:(fun acc fn ->
        (* find block associated with mem of sym *)
        let entry_block = Symtab.entry_of_fn fn in
        let sym_name = Symtab.name_of_fn fn in
        let symbols = Project.symbols project in
        let mem_bound = unstage (Symtab.create_bound symbols fn) in
        Main.analyze sym_name mem_bound entry_block project verbose @ acc) in
  let addrs = List.map result ~f:(fun x -> x.arg_addr) in
  let sym_and_bounds = List.map result ~f:(fun x ->
      let min = x.min_function_addr |> Word.to_int |> ok_exn in
      let max = x.max_function_addr |> Word.to_int |> ok_exn in
      (x.sym,min,max)) |> List.dedup in
  if String.length outfile > 0 then
    Main.write_output addrs outfile;
  if String.length sym_outfile > 0 then
    Main.write_output_syms sym_and_bounds sym_outfile;
  project

let () = Project.register_pass_with_args "arg_finder" main
