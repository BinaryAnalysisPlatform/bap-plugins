open Core_kernel.Std
open Bap.Std
open Project
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

open Custom_arm_abi

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
        | Bil.Int addr -> (match Memory.get ~scale ~addr project.base with
            | Ok w -> Bil.int w
            | _ -> exp)
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

  let output_rodata_tag proj =
    let res = Project.substitute proj in
    Memmap.iter res.memory ~f:(fun tag ->
        match Value.get comment tag with
        | None -> ()
        | Some text -> if text = ".rodata" then Format.printf "  [%s]" text)

  let output_rodata project exp =
    match exp with
    | Bil.Int addr ->
      begin
        Memory.view ~from:addr project.base ~words:4 |> function
        | Ok mem ->
          let memory =
            let tag = Value.create comment "$region" in
            Memmap.add project.memory mem tag in
          output_rodata_tag {project with memory}
        (* this is the case where we can't dereference memory, so
         * just return unit *)
        | Error e -> ()
      end
    | _ -> ()

  (* Gets the ABI information of a [dest_block]. The custom ABI module
   * only returns ABI information here if it's one of the funtions we
   * are interested in, e.g. __strcpy_chk *)
  let handle_dest block dest_block project parent_sym parent_mem acc verbose =
    let res = Table.find_addr project.symbols @@ Block.addr dest_block in
    Option.fold ~init:acc res ~f:(fun acc (mem, target) ->
        (* if the target matches strcpy, etc., we want to find its args *)
        let abi = new Custom_arm_abi.custom ~sym:target mem dest_block in
        let args = abi#args in
        if List.length args > 0 then
          begin
            let sym_name = List.hd_exn args |> fst in
            if verbose then
              Format.printf "\nFunc %s\n" @@ Option.value sym_name ~default:"";
            let extract_args = List.map args ~f:snd in
            let result = get_stmts block extract_args project in
            List.foldi ~init:acc result ~f:(fun i acc (mem,exp) ->
                if verbose then begin
                  let hex = Addr.to_int (min mem) |> ok_exn in
                  Format.printf "Arg %d at %x" i hex;
                  output_rodata project exp;
                  print_newline ();
                end;
                let st = Disasm.insn_at_addr project.disasm (min mem) in
                match st with
                | Some (mem,insn) ->
                  (* symbol, address of the arg of interest,
                     min and max addr of this function *)
                  let res = {sym = parent_sym;
                             arg_addr = min mem;
                             min_function_addr = min parent_mem;
                             max_function_addr = min parent_mem} in
                  res :: acc
                | None -> acc)
          end
        else acc)

  (* Gets blocks which are associated with calls and passes
   * these on to attempt retrieval of args *)
  let analyze sym bound entry project verbose =
    let blocks = Block.dfs ~bound entry in
    Seq.fold ~init:[] blocks ~f:(fun acc block ->
        Seq.fold ~init:acc (Block.dests block) ~f:(fun acc dests ->
            match dests with
            | `Unresolved _ -> acc
            | `Block (_, `Cond) -> acc
            | `Block (_, `Fall) -> acc
            | `Block (dest_block, `Jump) ->
              handle_dest block dest_block project sym bound acc verbose))
end

let main args project =
  let module Target = (val target_of_arch project.arch) in
  let module Main = Main(Target) in
  let outfile,sym_outfile,verbose = Cmdline.parse args in
  let result =
    Table.foldi ~init:[] project.symbols ~f:(fun mem sym acc ->
        (* find block associated with mem of sym *)
        match Table.find (Disasm.blocks project.disasm) mem with
        | None -> Format.eprintf "Symbol %a undefined!\n" String.pp sym; acc
        | Some entry_block ->
          Main.analyze sym mem entry_block project verbose @ acc) in
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

let () = register_plugin_with_args main
