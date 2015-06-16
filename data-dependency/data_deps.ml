open Core_kernel.Std
open Bap.Std
open Project
open Format
open Option

module Cmdline = struct
  open Cmdliner

  let info =
    let doc =
      "Get data dependencies of an instruction" in
    Term.info ~doc "Data dependencies"

  let infile : string Term.t =
    let doc = "New line separated list of addresses for which we want to
      determine data dependencies" in
    Arg.(value & opt string "" & info ["infile"] ~doc)

  let idascript : string Term.t =
    let doc = "Output an IDA script.py that highlights statements under
    consideration in yellow, and data dependencies in blue. WARNING: does not
    distinguish between instructions that share dependencies." in
    Arg.(value & opt string "" & info ["idascript"] ~doc)

  let addrs_from_file infile =
    In_channel.with_file infile ~f:(fun chan -> In_channel.input_lines chan) |>
    List.map ~f:(fun line ->  Int.of_string ("0x"^line))

  let process_args infile idascript =
    let addrs = addrs_from_file infile in
    addrs,idascript

  let parse argv =
    Term.eval ~argv (Term.(pure process_args $infile $idascript), info)
    |> function
    | `Ok x -> x
    | _ -> exit 1
end

exception Not_found of string
exception Can't_handle of string

type data_dependency = {
  (* stmt for which we hold dependencies *)
  stmt : Dataflow.Address.t * Stmt.t;
  (* list of dependencies *)
  deps : (Dataflow.Address.t * Stmt.t) list;
  (* the function symbol associated with this stmt *)
  sym : Symtab.fn
}

(* Reaching definitions visitor for dataflow -- we use this for dependencies *)
let reaching = object inherit [Domain.t list Var.Map.t] Dataflow.visitor
  method! enter_move var exp (address, dataflow, state, accum) =
    (* reaching definitions up to the point before the next insn *)
    Dataflow.set_bil dataflow address accum;
    let domain = Dataflow.find dataflow address in
    let diff = match Var.name var with
      | "mem" -> accum
      | _ -> Option.value (Var.Map.find state var) ~default:[] |>
             List.fold ~init:accum ~f:(fun accum value ->
                 Domain.diff accum value) in
    let result = Domain.union domain diff in
    let data = match Var.Map.mem state var with
      | false -> [domain]
      | true -> domain :: Var.Map.find_exn state var in
    (address, dataflow, Var.Map.add state ~key:var ~data, result)
end

(* Give the bil statement associated with the Dataflow [addr] *)
let stmt_from_addr disasm addr =
  match Dataflow.Address.to_bil addr disasm with
  | Some stmt -> stmt
  | None ->  raise (Not_found "No statement from addr!")

(* Returns the use-def chain for [insn_addr]. One use -> Many defs *)
let get_deps dataflow project insn_addr =
  (* get reaching defs for the BIL instruction at [insn addr] *)
  let defs = Dataflow.get_bil dataflow insn_addr |> List.map ~f:(fun addr ->
      (addr, stmt_from_addr project addr)) in
  (* [result] stores those reaching definitions where the var matches the
   * expression operand(s) of our [bil_insn] obtained from [insn_addr] *)
  let bil_insn = stmt_from_addr project insn_addr in
  match bil_insn with
  (* bil to mov *)
  | Bil.Move (var, exp) ->
    let operands =
      (object inherit [Var.t list] Bil.visitor
        method! enter_var v ll =
          v::ll
      end)#visit_exp exp [] in
    let result = List.filter defs ~f:(fun (_, stmt) ->
        match stmt with
        | Bil.Move (var, exp) -> List.exists operands ~f:(fun op ->
            Var.(var = op))
        | _ -> false) in
    List.map result ~f:(fun x -> fst x) |>
    (* TODO I believe the following line is not needed any more *)
    List.filter ~f:(fun x -> if x = insn_addr then false else true)
  | _ -> raise (Not_found "Not a move!")

(* This joins the use-def chains to get all dependencies *)
let get_dep_chain dataflow disasm start =
  let rec dc start acc =
    let res = get_deps dataflow disasm start in
    match res with
    | [] -> acc
    | l -> List.fold l ~init:acc ~f:(fun acc x ->
        if not (List.exists acc ~f:(fun addr -> addr = x)) then
          dc x (x::acc)
        else acc) in
  (* TODO List.dedup is probably not necessary *)
  dc start [] |> List.dedup

let deps_for_insn dataflow disasm insn_addr =
  get_dep_chain dataflow disasm insn_addr |>
  List.map ~f:(fun addr -> (addr, stmt_from_addr disasm addr))

let strip = String.filter ~f:(fun x -> x <> '\n')

let int_of_dataflow_addr addr =
  addr |> Dataflow.Address.mem |> Word.to_int |> ok_exn

let dataflow_addr_of_addr ?(idx=0) addr =
  Dataflow.Address.create addr idx

let print_header insn_addr addr =
  Format.printf "\nSTART <Data flow Dependence> <%a> %s\n"
    Dataflow.Address.pp insn_addr @@ strip @@ Stmt.to_string addr

let print_dependencies deps =
  List.iter deps ~f:(fun (addr, stmt) ->
      Format.printf "\t<%a> %s\n"
        Dataflow.Address.pp addr @@ strip @@ Stmt.to_string stmt)

(* Not used, useful for debugging and inspection *)
let output_verbose_reaching_defs disasm dataflow =
  Format.printf "START <Reaching Definitions>";
  List.iter (Dataflow.get_all_bils dataflow) ~f:(fun addr ->
      let stmt = stmt_from_addr disasm addr in
      Format.printf "<%a> %s\n"
        Dataflow.Address.pp addr @@ strip @@ Stmt.to_string stmt;
      let defs = Dataflow.get_bil dataflow addr in
      List.iter (List.sort defs ~cmp:Dataflow.Address.compare) ~f:(fun addr ->
          let stmt = stmt_from_addr disasm addr in
          Format.printf "\t<%a> %s\n"
            Dataflow.Address.pp addr @@ strip @@ Stmt.to_string stmt))

let output_verbose_data_deps disasm dependency =
  Format.printf "\nFunction %s\n" @@ Symtab.name_of_fn dependency.sym;
  let addr,header = dependency.stmt in
  print_header addr header;
  print_dependencies dependency.deps

(* Populate the data dependency structure with information, given an addr *)
let collect_data_deps disasm dataflow func addr =
  let daddr = dataflow_addr_of_addr addr in
  let stmt = stmt_from_addr disasm daddr in
  let deps = deps_for_insn dataflow disasm daddr in
  let t = (daddr,stmt) in
  {stmt = t; deps; sym = func}

(* Run reaching definitions and then collect the data dependenices *)
let process_addr project addr =
  try
    let symtab = Project.symbols project in
    let func =
      match Symtab.fns_of_addr symtab addr with
      | [] -> raise (Not_found (sprintf "No function found containing addr %S"
                                @@ Addr.to_string addr))
      | [f] -> f
      | _ -> raise (Can't_handle (sprintf "More than one function for addr %S,\
                                           can't handle this!" @@
                                  Addr.to_string addr)) in
    let entry = Symtab.entry_of_fn func in
    let bound = unstage (Symtab.create_bound symtab func) in
    let dataflow =
      Dataflow.create ~entry ~bound ~interior:Domain.empty
        ~boundary:Domain.empty ~direction:Dataflow.Forwards in
    Dataflow.run dataflow ~worklist:None ~meet:Domain.union
      ~user_state:Var.Map.empty ~transfer:reaching;
    collect_data_deps (Project.disasm project) dataflow func addr
  with
  | Not_found msg -> print_endline msg; exit 1

let ida_stmt_higlight addr =
  sprintf "idaapi.set_item_color(DecodeInstruction(0x%x).ea, 0xccffff)\n"
    ((fst addr.stmt) |> int_of_dataflow_addr)

let ida_dependency_highlight addr =
  sprintf "idaapi.set_item_color(DecodeInstruction(0x%x).ea, 0xffffd0)" addr

(* output a script which highlights the instructions that are
 * data dependencies of our statement of interest *)
let output_script idascript result =
  Out_channel.with_file idascript ~f:(fun chan ->
      Out_channel.output_string chan "Wait()\n";
      List.iter result ~f:(fun x ->
          Out_channel.output_string chan @@ ida_stmt_higlight x;
          let int_addrs = List.map x.deps ~f:(fun (addr,_stmt) ->
              int_of_dataflow_addr addr) in
          Out_channel.output_lines chan
            (int_addrs |> List.dedup |> List.map ~f:(fun x ->
                 ida_dependency_highlight x))))

let annotate disasm mem entry =
  let header_mem = Dataflow.mem_from_dataflow_addr disasm @@ fst entry.stmt in
  let mem = Memmap.add mem header_mem (Value.create color `yellow) in
  let deps_mem = List.map entry.deps ~f:(fun x ->
      Dataflow.mem_from_dataflow_addr disasm @@ fst x) |> List.dedup in
  List.fold deps_mem ~init:mem ~f:(fun mem x ->
      Memmap.add mem x (Value.create color `blue))

let main args project =
  let addrs_of_interest,idascript = Cmdline.parse args in
  let result =
    List.fold ~init:[] addrs_of_interest ~f:(fun acc x ->
        let addr = Addr.of_int ~width:32 x in
        (process_addr project addr) :: acc) in
  List.iter result ~f:(fun x -> output_verbose_data_deps (Project.disasm project) x);
  if String.length idascript > 0 then output_script idascript result;
  let memory = List.fold result ~init:(Project.memory project)
      ~f:(fun mem entry -> annotate (Project.disasm project) mem entry) in
  Project.with_memory project memory

let () = Project.register_pass_with_args "data_deps" main
