open Core_kernel.Std
open Bap.Std
open Format
open Regular.Std

include Self ()
open Microx.Std
module SM = Monad.State
open SM.Monad_infix

open Options
open State_data

let equal = Polymorphic_compare.equal

class memory : Bil.storage =
  object(self : 's)
    val storage = Bitvector.Map.empty

    method save x u =
      {< storage = Map.add storage ~key:x ~data:u >}

    method load x =
      Map.find storage x
  end

class base_context p options = object(self : 's)
  inherit Biri.context p

  (** Use local callstack *)
  val cstack : sub term list = []
  val blk : blk term option = None

  (** Track addrs in memory map *)
  val addrs = []
  val path_count = 0
  val branch_count = 0
  val blk_count = 0
  val step_count = 0

  method blk_count = blk_count
  method step_count = step_count
  method cstack = cstack
  method path_count = path_count

  method inc_blk_count = {< blk_count = blk_count +1 >}
  method inc_step_count = {< step_count = step_count +1 >}

  method update_callstack_enter sub = {< cstack = sub :: cstack >}

  method update_callstack_leave (_ : sub term) = match cstack with
    | _ :: top -> {< cstack = top >}
    | [] -> {< cstack = [] >}

  method addrs = addrs
  method branch_count = branch_count

  method add_addr (a : Word.t) = {< addrs = a :: addrs>}

  (** Logging *)

  method private dump_storage ~invoker memory  =
    let open State_data in
    let data = List.map self#addrs ~f:(fun addr ->
        match memory#load addr with
        | Some w -> [sprintf "%a" Addr.pps addr; sprintf "%a" Word.pps w]
        | None -> []) in
    State_data.to_csv ?dirname:options.dir ~invoker `Memory step_count data

  method dump_memory ~invoker =
    self#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
        match Bil.Result.value bil_result with
        | Bil.Mem s -> self#dump_storage ~invoker s
        | _ -> ())

  method dump_bindings ~invoker =
    let data =
      Seq.to_list (self#bindings) |>
      List.map ~f:(fun (v,bil_result) ->
          match Bil.Result.value bil_result with
          | Bil.Imm w ->
            [sprintf "%a" Var.pps v;
             sprintf "%a" Word.pps w]
          | Bil.Bot -> [sprintf "%a" Var.pps v; "_|_"]
          | Bil.Mem _ -> []) in
    State_data.to_csv ?dirname:options.dir ~invoker `Regs step_count data

  method dump_trace ~invoker =
    let trace = List.map self#trace
        ~f:(fun tid -> [sprintf "\t%a%!" Tid.pps tid]) in
    State_data.to_csv ?dirname:options.dir ~invoker `Trace self#step_count trace

  method dump_dot =
    let sub = self#cstack |> List.hd_exn in
    let pc = List.hd self#trace in
    State_data.to_dot ?pc ?dirname:options.dir sub self#step_count
      self#trace

  method dump_path_counts =
    State_data.to_csv ?dirname:options.dir `Path_counts self#step_count
      [[Int.to_string self#path_count]]

  method dump_extras extras =
    State_data.to_csv ?dirname:options.dir `Myself self#step_count [extras]

  method log ~extras (invoker : Flag.hook) =
    let open Flag in
    let if_active hl f = if List.mem ~equal hl invoker then Lazy.force f in
    List.iter options.directives ~f:(fun (Flag (d,hl)) ->
        match d with
        | `Memory -> if_active hl (lazy (self#dump_memory ~invoker))
        | `Regs -> if_active hl (lazy (self#dump_bindings ~invoker))
        | `Dot -> if_active hl (lazy (self#dump_dot))
        | `Path_counts -> if_active hl (lazy (self#dump_path_counts ~invoker))
        | `Trace -> if_active hl (lazy (self#dump_trace ~invoker))
        | `Myself -> if_active hl (lazy (self#dump_extras extras ~invoker))
        | _ -> ()
      )
end

class debugger_context ?max_steps ?max_loop p options = object(self : 's)
  inherit base_context p options as base
  inherit Conqueror.context ?max_steps ?max_loop p as super

  method! add_checkpoint tid =
    let result = super#add_checkpoint tid in
    result

  (** must merge blk_counts, etc. from incoming *)
  method merge_counts ctxt =
    {< branch_count = ctxt#branch_count+1;
       blk_count = ctxt#blk_count;
       step_count = ctxt#step_count;
       path_count = ctxt#path_count >}

  method! merge ctxt =
    let self = super#merge ctxt in
    self#merge_counts ctxt

  method update_path_count =
    self#log ~extras:[""] `Path_terminates;
    {< path_count = path_count + 1 >}

  method! return =
    let result = super#return in
    result#update_path_count

  method! backtrack =
    let result = super#backtrack in
    match result with
    | None ->
      result
    | Some result -> Some (result#update_path_count)

  method! log ~extras (invoker : Flag.hook) =
    let open Flag in
    base#log ~extras invoker;
    let if_active hl f = if List.mem ~equal hl invoker then Lazy.force f in
    List.iter options.directives ~f:(fun (Flag (d,hl)) ->
        match d with
        | `Checkpoints -> if_active hl (lazy (self#dump_checkpoints ~invoker))
        | _ -> ()
      )

  method dump_checkpoints =
    let data =
      (* key: sub data: tids *)
      Map.fold ~init:[] ~f:(fun ~key ~data acc ->
          (* key: tids data: ctxt *)
          let res =
            Map.fold ~init:[] data ~f:(fun ~key:key' ~data acc' ->
                [sprintf "%a : %a" Tid.pps key Tid.pps key']::acc') in
          res@acc) self#checkpoints in
    State_data.to_csv ?dirname:options.dir `Checkpoints
      self#step_count data
end

let memory_lookup proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.hd |> function
  | None -> None
  | Some (mem,_) -> match Memory.get ~addr mem with
    | Ok w -> Some w
    | _ -> None

let register_lookup proj =
  let arch = Project.arch proj in
  let width = Arch.addr_size arch |> Size.in_bits in
  let mem_start = Word.of_int64 ~width 0x40000000L in
  let module Target = (val target_of_arch arch) in
  fun var -> Option.some_if (Target.CPU.is_sp var) mem_start

class ['a] concretizer_debugger ?deterministic ?random_seed ?reg_policy
    ?mem_policy proj options =
  let memory = memory_lookup proj in
  let lookup = register_lookup proj in
  object(self)
    constraint 'a = debugger_context
    inherit ['a] Concretizer.main ~memory ~lookup
        ?random_seed ?reg_policy ?mem_policy () as concrete

    method! lookup v =
      concrete#lookup v >>= fun r ->
      let extras =
        (match Bil.Result.value r with
         | Bil.Imm w -> [sprintf "Lookup %a = %a" Var.pps v Word.pps w]
         | _ -> [sprintf "Lookup %a" Var.pps v]) in
      SM.get () >>= fun ctxt ->
      ctxt#log ~extras `Lookup;
      SM.return r

    method! load s a =
      concrete#load s a >>= fun r ->
      let extras = [sprintf "Load %a\n" Addr.pps a] in
      SM.get () >>= fun ctxt ->
      ctxt#log ~extras `Load;
      SM.return r

    method! store s a w =
      concrete#store s a w >>= fun r ->
      SM.update (fun ctxt -> ctxt#add_addr a) >>= fun () ->
      let extras = [sprintf "Store %a : %a" Addr.pps a Word.pps w] in
      SM.get () >>= fun ctxt ->
      ctxt#log ~extras `Store;
      SM.return r

    method! update v r =
      concrete#update v r >>= fun () ->
      let extras = [sprintf "Update %a : %a" Var.pps v Bil.Result.pps r] in
      SM.get () >>= fun ctxt ->
      match List.hd ctxt#trace with
      | None -> SM.return ()
      | Some tid ->
        ctxt#log ~extras `Update;
        SM.return ()

    method! undefined_addr a =
      concrete#undefined_addr a >>= fun r ->
      let extras = [sprintf "Undefined addr %a" Addr.pps a] in
      SM.get () >>= fun ctxt ->
      ctxt#log ~extras `Undefined_addr;
      SM.return r

    method! undefined_var v =
      concrete#undefined_var v >>= fun r ->
      SM.get () >>= fun ctxt ->
      let extras = [sprintf "Undefined var %a" Var.pps v] in
      ctxt#log ~extras `Undefined_var;
      SM.return r
  end

class ['a] conqueror_debugger ?deterministic proj =
  let prog = Project.program proj in
  object(self)
    inherit ['a] Conqueror.main ?deterministic prog as super

    method! eval_def def =
      SM.get () >>= fun ctxt ->
      let extras = [sprintf "Def %a" Def.pps def] in
      ctxt#log ~extras `Eval_def;
      super#eval_def def

    method! eval_jmp jmp =
      SM.get () >>= fun ctxt ->
      let extras = [sprintf "%a" Jmp.pps jmp] in
      ctxt#log ~extras `Eval_jmp;
      super#eval_jmp jmp

    method! eval_blk blk =
      SM.get () >>= fun ctxt ->
      let extras = [sprintf "%a" Blk.pps blk] in
      ctxt#log ~extras `Eval_blk;
      SM.update (fun ctxt -> ctxt#inc_blk_count) >>= fun () ->
      super#eval_blk blk >>= fun () ->
      SM.return ()

    method! eval_sub sub =
      SM.update (fun ctxt -> ctxt#update_callstack_enter sub) >>= fun () ->
      let extras = [sprintf "%a" Sub.pps sub] in
      SM.get () >>= fun ctxt ->
      ctxt#log ~extras `Eval_sub;
      super#eval_sub sub >>= fun () ->
      SM.update (fun ctxt -> ctxt#update_callstack_leave sub)

    method! enter_term cls t =
      SM.get () >>= fun ctxt ->
      let extras =
        [Term.switch cls t
           ~program:(sprintf "%a" Program.pps)
           ~sub:(sprintf "%a" Sub.pps)
           ~arg:(sprintf "%a" Arg.pps)
           ~blk:(sprintf "%a" Blk.pps)
           ~phi:(sprintf "%a" Phi.pps)
           ~def:(sprintf "%a" Def.pps)
           ~jmp:(sprintf "%a" Jmp.pps)] in
      ctxt#log ~extras `Enter_term;
      SM.update (fun ctxt -> ctxt#inc_step_count) >>= fun () ->
      super#enter_term cls t
  end

class ['a] debugger ?deterministic ?random_seed ?reg_policy ?mem_policy
    proj options =
  object(self)
    constraint 'a = #debugger_context
    inherit ['a] conqueror_debugger ?deterministic proj
    inherit ['a] concretizer_debugger ?random_seed ?reg_policy
        ?mem_policy proj options

    method! empty = new memory
  end

let run proj sub options =
  let max_trace = 1_000_000 in
  let max_loop = 10 in
  let deterministic = false in
  let reg_policy = `Random in
  let mem_policy = `Random in
  let p = Project.program proj in
  let ctxt = new debugger_context ~max_steps:max_trace ~max_loop p options in
  let biri = new debugger
    ~deterministic
    ~reg_policy
    ~mem_policy proj options in
  let res = biri#eval_sub sub in
  Monad.State.exec res ctxt |> fun ctxt -> ()

let main proj =
  let options = Cmdline.parse argv in
  match options.fname with
  | Some fname ->
    (let tid = Tid.from_string_exn ("@"^fname) in
     Program.lookup sub_t (Project.program proj) tid |> function
     | Some sub -> run proj sub options |> ignore
     | None -> ())
  | None -> ()

let () =
  Project.register_pass' main
