open Core_kernel.Std
open Bap.Std
open Format

(* MEMORY *)
(* ------ *)
(** return the memory that spans a section, such as '.rodata' *)
let find_section_by_name project name =
  let memory = Project.memory project in
  Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
      Option.(Value.get Image.section x >>= fun n ->
              Option.some_if (n = name) m))

(** print all memory of a binary, with labels *)
let print_memory project () =
  Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
      printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem)

(* BIR *)
(* --- *)

let target_tid_of_call call =
  match Call.target call with
  | Direct tid -> Some tid
  | _ -> None

let get_return_target (t : call) =
  Option.(Call.return t >>= function
    | Direct tid -> Some tid
    | _ -> None)

let calls_of_blk blk =
  Term.enum jmp_t blk |> Seq.fold ~init:[] ~f:(fun acc jmp ->
      match Jmp.kind jmp with
      | Call t ->
        t::acc
      | _ -> acc)

(** Returns the jmp term associated with call, and the call *)
let calls_of_blk_with_tid blk =
  Term.enum jmp_t blk |> Seq.fold ~init:[] ~f:(fun acc jmp ->
      match Jmp.kind jmp with
      | Call t ->
        (Term.tid jmp,t)::acc
      | _ -> acc)

let print_bir project () =
  Format.printf "%a\n" Program.pp @@ Project.program project
(** And so forth for Sub.pp, etc. *)

let sub_name_of_tid = function
  | Direct tid ->
    Some (Tid.name tid)
  | _ -> None

let sub_of_tid project tid =
  Option.(Program.lookup sub_t (Project.program project) tid)

(** Now returns with '@' for symbols *)
let resolve_jmp_name jmp =
  match Jmp.kind jmp with
  | Call c ->
    sub_name_of_tid (Call.target c)
  | _ -> None

  let val_exn v = Option.value_exn v

(** expects @ in name *)
let sub_from_name project fun_name =
  let open Or_error in
  match Tid.from_string fun_name with
  | Ok tid -> Term.find sub_t (Project.program project) tid
  | Error err -> None

(** A path is terminated if there's no successor *)
(** extracts a successor's tid from a jump term *)
let succ_tid_of_jmp jmp : tid option =
  match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | Call t -> Option.(Call.return t >>= function
    | Direct tid -> Some tid
    | _ -> None)
  | Int (_,tid) -> Some tid
  | _ -> None

(** resolves jump tid to block tid using find *)
let succ_of_jmp sub jmp =
  match succ_tid_of_jmp jmp with
  | Some tid as blk when Term.find blk_t sub tid <> None -> blk
  | _ -> None

let succs_of_blk sub blk =
  Term.enum jmp_t blk |>  Seq.filter_map ~f:(succ_of_jmp sub)

let succs_of_blk_tid sub blk_tid =
  Option.(Term.find blk_t sub blk_tid >>= fun blk ->
          Some (succs_of_blk sub blk)) |> Option.value ~default:Seq.empty

let blk_of_tid sub blk_tid =
  match Term.find blk_t sub blk_tid with
  | Some blk -> blk
  | None -> failwith
    @@ sprintf "Cannot get blk from tid: %s" @@ Tid.name blk_tid

let tid_of_blk blk = Term.tid blk

let tid_of_sub sub = Term.tid sub

let contains_indirect_goto blk =
  let jmps = Term.enum jmp_t blk in
  Seq.exists jmps ~f:(fun jmp ->
      match Jmp.kind jmp with
      | Goto label ->
        begin
          match label with
          | Indirect _ -> true
          | _ -> false
        end
      | _ -> false)

let ends_with_lr blk =
  let jmps = Term.enum jmp_t blk in
  Seq.exists jmps ~f:(fun jmp ->
      match Jmp.kind jmp with
      | Ret label ->
        begin match label with
          | Indirect _ -> true
          | _ -> false
        end
      | _ -> false)

(** TODO this really should return tids... *)
let blks_with_calls sub =
  Term.enum blk_t sub |> Seq.filter_map ~f:(fun blk ->
      match calls_of_blk blk with
      | [] -> None
      | _ -> Some (Term.name blk))
  |> Seq.to_list

(** sub tids that this sub calls *)
let calls_of_sub sub =
  Term.enum blk_t sub |> Seq.to_list |> List.fold ~init:[] ~f:(fun acc blk ->
      let tids =
        List.filter_map (calls_of_blk blk) ~f:(fun c ->
            match Call.target c with
            | Direct tid -> Some tid
            | _ -> None) in
      acc @ tids)

let sub_of_tid project tid =
  Term.find sub_t (Project.program project) tid

(** Is it an indirect jump, with only one jump? TODO: why does this
    not catch lr case? *)
let get_exit_blocks sub =
  Term.enum blk_t sub |> Seq.filter ~f:contains_indirect_goto |>
  Seq.filter ~f:(fun b ->
      Seq.length @@ Term.enum jmp_t b = 1)

let is_exit_block sub blk = Seq.is_empty (succs_of_blk sub blk)

(** clone sub with new tids, and a table that maps old tids to new
    ones *)
let deep_clone_sub sub =
  let tids = Tid.Table.create () in
  let add_and_clone term =
    let res = Term.clone term in
    Tid.Table.set tids ~key:(Term.tid term) ~data:(Term.tid res);
    res in
  let cloned_sub =
    Term.clone sub |>
    Term.map blk_t ~f:(fun blk ->
        add_and_clone blk |>
        Term.map phi_t ~f:add_and_clone |>
        Term.map def_t ~f:add_and_clone |>
        Term.map jmp_t ~f:add_and_clone) |>
    Term.map arg_t ~f:add_and_clone in
  cloned_sub,tids

let calls_of_blk_str blk =
  let jmps = Term.enum jmp_t blk in
  Seq.filter_map jmps ~f:resolve_jmp_name |> Seq.to_list

let contains_call blk call_name =
  let jmps = Term.enum jmp_t blk in
  let names = Seq.filter_map jmps ~f:resolve_jmp_name in
  Seq.exists names ~f:(fun x -> x = call_name)

let calls_self sub =
  let blk_tids = blks_with_calls sub in
  List.exists blk_tids ~f:(fun t ->
      let blk = blk_of_tid sub @@ Tid.(!t) in
      match calls_of_blk_with_tid blk with
      | [] -> false
      | [(lhs_tid, call)] ->
        let call_target = Label.to_string @@ Call.target call in
        call_target = ("@"^(Sub.name sub))
      | _ -> failwith @@ sprintf "This blk has more than one call: %s" @@
        Blk.to_string blk)

let is_mutually_recursive program =
  let callgraph = Program.to_graph program in
  let scc_partition = Graphlib.strong_components
      (module Graphlib.Callgraph) callgraph in
  Seq.filter (Partition.groups scc_partition) ~f:(fun group ->
      let g = Group.enum group in
      Seq.length g > 1) |> Seq.length > 0

let rec recursively_add project builder sub_tid existing =
  let sub = sub_of_tid project sub_tid |> val_exn in
  Program.Builder.add_sub builder sub;
  let existing = Tid.Set.add existing sub_tid in
  let calls_of_sub = calls_of_sub sub |> List.dedup in
  let new_tids =
    List.filter calls_of_sub ~f:(fun tid ->
        not (Tid.Set.mem existing tid)) in
  match new_tids with
  | [] -> existing
  | l -> List.fold l ~init:existing ~f:(fun existing tid ->
      recursively_add project builder tid existing)

  (** Produce a callgraph rooted at sub *)
(** add subroutine to set, add to builder, get calls for each call
    not in set, add subroutine *)
let callgraph_of_sub project sub_tid =
  let open Option in
  let existing = Tid.Set.empty in
  let builder = Program.Builder.create () in
  sub_of_tid project sub_tid >>= (fun sub ->
      recursively_add project builder sub_tid existing |> some) |> ignore;
  Program.Builder.result builder

(* MAPPERS *)
(* ------- *)

(** Bil mapper. Takes a stmt list and gives you back a stmt list.
    This one resolves memory. *)
let resolve_indirects project =
  Bil.map (object inherit Bil.mapper as super
    method! map_load ~mem ~addr endian scale =
      let exp = super#map_load ~mem ~addr endian scale in
      match addr with
      | Bil.Int addr ->
        let exp = Memmap.lookup (Project.memory project) addr |> Seq.hd |>
                  function
                  | None -> exp
                  | Some (mem,_) -> match Memory.get ~scale ~addr mem with
                    | Ok w -> Bil.int w
                    | _ -> exp in
        exp
      | _ -> exp
  end)

(** Exp mapper. Everything is the same as the above, but we use
    Exp.map! *)
let resolve_indirects project =
  Bil.map (object inherit Bil.mapper as super
    method! map_load ~mem ~addr endian scale =
      let exp = super#map_load ~mem ~addr endian scale in
      match addr with
      | Bil.Int addr ->
        let exp = Memmap.lookup (Project.memory project) addr |> Seq.hd |>
                  function
                  | None -> exp
                  | Some (mem,_) -> match Memory.get ~scale ~addr mem with
                    | Ok w -> Bil.int w
                    | _ -> exp in
        exp
      | _ -> exp
  end)


(* CONSTANT FOLDING *)
(* ---------------- *)

(** Note use of Term.enum *)
let print_constant_fold_sub sub =
  match Term.first blk_t sub with
  | None -> ()
  | Some entry_blk ->
    Term.enum def_t entry_blk |> (** Extracts definitions from blocks *)
    Seq.iter ~f:(fun def ->
        let rhs_exp = Def.rhs def in
        let folded_rhs_exp = Exp.fold_consts rhs_exp in
        Format.printf "Before: %a\nAfter: %a\n" Exp.pp rhs_exp Exp.pp folded_rhs_exp)

(** Same as above, but with (destructive) update *)
(** There are two approaches: fold over a mutable state will
    changing terms and adding them; alternatively just update *)

(** This is only for the first block of the subroutine (example). If
    there is no first block, return a blank blk *)
let constant_fold_sub sub =
  match Term.first blk_t sub with
  | None -> Blk.create ()
  | Some entry_blk ->
    Term.map def_t entry_blk ~f:(fun def ->
        let rhs = Def.rhs def in
        Def.with_rhs def (Exp.fold_consts rhs))

(** Term.map for constant folding *)
let print_constant_fold project () =
  (** extract subroutines as sequence from !project *)
  Term.enum sub_t (Project.program project) |>
  Seq.iter ~f:print_constant_fold_sub (** constant fold each sub *)

(** Constant fold the first block of each subroutine *)
let constant_fold project () =
  Term.enum sub_t (Project.program project) |>
  Seq.map ~f:constant_fold_sub

    (** make calls to exported functions explicit (libmad) *)
let make_exported_calls_explicit project =
  let project' =
    Project.with_program project
      (Project.program project |> Term.map sub_t ~f:(fun sub ->
           Term.map blk_t sub ~f:(fun blk ->
               Term.map jmp_t blk ~f:(fun jmp ->
                   match Jmp.kind jmp with
                   | Call c ->
                     (match Call.target c with
                      | Direct tid ->
                        if String.is_prefix (Tid.name tid) ~prefix:"@." then
                          let s = String.substr_replace_first
                              (Tid.name tid) ~pattern:"@." ~with_:"@" in
                          sub_from_name project s
                          |> function
                          | Some sub ->
                            let tid = tid_of_sub sub in
                            (*Format.printf "Replacement: %s\n" (Tid.name tid);*)
                            Jmp.create_call @@
                            Call.with_target c (Label.direct tid)
                          | None -> jmp
                        else jmp
                      | _ -> jmp)
                   | _ -> jmp)))) in
  project'

  (** Give fallthrough edges explicit jmp conditions if there are exactly
      two jmps *)
let make_implicit_jmp_conds_explicit project =
  let project' =
    Project.with_program project
      (Project.program project |> Term.map sub_t ~f:(fun sub ->
           Term.map blk_t sub ~f:(fun blk ->
               let jmps = Term.enum jmp_t blk in
               match Seq.length jmps with
               | 0 -> blk
               (** Weird thing: only one jmp with a condition. Can
                   happen with interrupts. Just leave this alone, it
                   probably won't occur on a path we care about.  *)
               | 1 ->
                 (*
                 let jmp = Seq.hd_exn jmps in
                 let exp = Jmp.cond jmp in
                 if exp <> Bil.int (Word.one 1) then
                   Format.printf
                     "Warning: Exp has condition: \"%s\", \
                      but only one jmp for this block:\ %s\n"
                     (Exp.to_string exp) (Blk.to_string blk);*)
                 blk
               | 2 ->
                 let jmp1 = Seq.tl_eagerly_exn jmps |> Seq.hd_exn in
                 let jmp2 = Seq.hd_exn jmps in
                 (match (Jmp.cond jmp1, Jmp.cond jmp2) with
                  | (e1,e2) when e1 = Bil.int Word.b1 ->
                    Term.map jmp_t blk ~f:(fun jmp ->
                        if Term.tid jmp = Term.tid jmp1 then
                          Jmp.with_cond jmp (Bil.unop Bil.not (Jmp.cond jmp2))
                        else
                          jmp)
                  | (e1,e2) when e2 = Bil.int Word.b1 ->
                    Term.map jmp_t blk ~f:(fun jmp ->
                        if Term.tid jmp = Term.tid jmp2 then
                          Jmp.with_cond jmp (Bil.unop Bil.not (Jmp.cond jmp1))
                        else
                          jmp)
                  | (e1, e2) ->
                    Format.printf "Warning: There are two jmps, but \
                                   neither have a fallthrough condition:\
                                   %s %s\n" (Exp.to_string e1) (Exp.to_string e2);
                    blk)
               | _ ->
                 (** Weird thing: this happens when there are two jumps to constant addresses. Going
                     to ignore (hopefully safely) for now. *)
                 (*failwith @@ Format.sprintf "More than two jmps in this blk %s\n"
                   @@ Blk.to_string blk*)
                 blk))) in
  project'

(** TODO support arm and x86 *)
let make_call_returns_explicit sub =
  let blks_that_need_return =
    Term.enum blk_t sub |> Seq.fold ~init:Tid.Set.empty ~f:(fun acc blk ->
        if List.length @@ calls_of_blk blk > 0 then
          let next_blk =
            Term.prev blk_t sub (tid_of_blk blk) in
          match next_blk with
          | Some next_blk ->
            Set.add acc (tid_of_blk next_blk)
          | None -> acc
        else acc) in
  Term.map blk_t sub ~f:(fun blk ->
      match Set.mem blks_that_need_return (tid_of_blk blk) with
      | true ->
        let def = Def.create (AMD64.CPU.rax) (Bil.unknown "return_val" reg32_t) in
        Term.prepend def_t blk def
      | false -> blk)

  (* DIAGNOSTICS *)
  (* ----------- *)
  (** generally less useful. Just for sanity checks *)

  (** utility *)
  let print_jmp_summary project jmps all_subs =
    let prog = Project.program project in
    Seq.map jmps ~f:(fun jmp ->
        match Jmp.kind jmp with
        | Call c ->
          begin
            Call.target c |> function
            | Direct tid ->
              Seq.find_map (Term.enum sub_t prog) ~f:(fun sub ->
                  Option.some_if (tid = Term.tid sub) (Sub.name sub))
            | _ -> None
          end
        | Goto l ->
          Format.printf "Goto Target: %a\n" Label.pp l; None
        | Ret l -> Format.printf "ret\n"; None
        | Int (i,tid) -> Format.printf "int\n"; None)

(** sanity checker by using first term*)
let test_first_term project () =
  let prog = Project.program project in
  let first_sub = Term.first sub_t prog |> function
    | None -> failwith "No subroutines"
    | Some sub -> sub in
  let first_blk = Term.first blk_t first_sub |> function
    | None -> failwith "No blk"
    | Some blk -> Format.printf "%a\n" Blk.pp blk; blk in
  let jmps = Term.enum jmp_t first_blk in
  print_jmp_summary project jmps Seq.empty |> ignore

(** Test that we can resolve symbol calls such as printf *)
let resolve_symbols_of_calls project () =
  let prog = Project.program project in
  let all_subs = Term.enum sub_t prog in
  Seq.iter all_subs ~f:(fun sub ->
      let all_blks = Term.enum blk_t sub in
      Seq.iter all_blks ~f:(fun blk ->
          let jmps = Term.enum jmp_t blk in
          let names = Seq.map jmps ~f:resolve_jmp_name in
          Seq.iter names ~f:(function
              | Some name -> Format.printf ": %s\n" name
              | None -> ())))

(** TODO add check that confirms no back-edges are traversed *)
let num_paths_dag
    (module G : Graphlib.Graph with type edge = Graphlib.Tid.Tid.edge and
    type node = tid and type t = Graphlib.Tid.Tid.t)
    graph start_tid =
  let dp = Tid.Table.create () in
  let rec go curr : int =
    if (G.Node.succs curr graph |> Seq.length) = 0 then
      1
    else match Tid.Table.find dp curr with
      | Some s -> s
      | None ->
        let sum =
          Seq.fold ~init:0 (G.Node.succs curr graph)
            ~f:(fun sum child -> sum + go child) in
        Tid.Table.add_exn dp curr sum;
        sum
  in go start_tid


exception Timeout

let timeout_option ~secs ~f ~x =
  let b = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  if b <> Sys.Signal_default then failwith
      "timeout: Expected exclusive use of alarm signal";
  let cleanup () =
    let _ = Unix.alarm 0 in
    Sys.set_signal Sys.sigalrm b
  in
  try
    let old = Unix.alarm secs in
    if old <> 0 then failwith "timeout: Expected exclusive use of Unix.alarm";
    let o = (f x) in
    (* turn alarm off *)
    cleanup ();
    Some o
  with
  | Timeout -> cleanup (); None
  | e -> cleanup (); raise e

let timeout ~secs ~f ~x =
  let f = timeout_option ~secs ~f in
  match f ~x with
  | Some x -> x
  | None -> raise Timeout
