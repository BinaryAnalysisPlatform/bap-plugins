open Core_kernel.Std
open Option
open Bap.Std
open Microx.Std
open ARM
include Self()

open Format
module SM = Monad.State
open SM.Let_syntax
open SM.Monad_infix

open Options
open Uaf_error

let rand32 lo hi = Int32.(Random.int32 (hi+(hi-lo)) + lo)

let rec generate = function
  | `Fixed x -> Word.of_int32 x
  | `Random -> generate (`Interval (Int32.min_value, Int32.max_value))
  | `Interval (lo,hi) -> generate (`Fixed (rand32 lo hi))

type error = Uaf_error.t

(** Alloc'ed data info *)
module Alloc_data = struct
  type t = {min : addr;
            max : addr;
            size : word}
end


type t = string -> bool

let alloc_result_pat = [".*alloc.*result.*"]
let alloc_size_pat =  [".*alloc.*size.*"]
let free_pat =   [".*free.*ptr.*"]

let compile s =
  s |> List.map ~f:Re_posix.re
  |> Re.alt
  |> Re.compile
  |> Re.execp

let is_alloc_result = alloc_result_pat |> compile
let is_alloc_size = alloc_size_pat |> compile
let is_free = free_pat |> compile

let equal = Polymorphic_compare.equal

class context ?dir ?(directives=[]) ?max_steps ?max_loop prog =
  object(self : 's)

    inherit Debugger.conqueror_debugger_context
        ?dir ~directives ?max_steps ?max_loop prog as super

    (** Track addrs that have been free'd *)
    val freed_addrs = Tid.Map.empty
    val alloced_addrs = Tid.Map.empty

    val errors : error list = []

    (** getters *)
    method freed_addrs = freed_addrs
    method alloced_addrs = alloced_addrs
    method errors = errors

    (* currently works across all paths. TODO: add option for selecting only one
       path *)
    method add_error (error : error) : 's =
      let contains = List.exists self#errors ~f:(fun e ->
          error.use_tid = e.use_tid &&
          error.free_tid = e.free_tid &&
          error.alloc_tid = e.alloc_tid &&
          error.addr = e.addr) in
      if not contains then
        {< errors = error :: errors >}
      else self

    method dump_freed_addrs =
      let open Alloc_data in
      let data = Map.fold ~init:[] ~f:(fun ~key ~data acc ->
          ([sprintf "%a -> %a [%a]" Tid.pps key Addr.pps data.min Word.pps
              data.size])::acc) self#freed_addrs in
      State_data.to_csv ?dirname:dir `Freed_addrs self#step_count data

    method dump_alloced_addrs =
      let open Alloc_data in
      let data = Map.fold ~init:[] ~f:(fun ~key ~data acc ->
          ([sprintf "%a -> %a [%a]" Tid.pps key Addr.pps data.min Word.pps
              data.size])::acc) self#alloced_addrs in
      State_data.to_csv ?dirname:dir `Alloced_addrs self#step_count data

    method! log ~extras (invoker : Flag.hook) =
      let open Flag in
      super#log ~extras invoker;
      let if_active hl f = if List.mem ~equal hl invoker then Lazy.force f in
      List.iter directives ~f:(fun (Flag (d,hl)) ->
          match d with
          | `Alloced_addrs -> if_active hl (lazy (self#dump_alloced_addrs ~invoker))
          | `Freed_addrs -> if_active hl (lazy (self#dump_freed_addrs ~invoker))
          | _ -> ())

    method lookup_alloc_size =
      super#bindings |> Seq.fold ~init:None ~f:(fun acc (v,r) ->
          match Var.name v with
          | s when is_alloc_size s ->
            (match Bil.Result.value r with
             | Bil.Imm w -> Some w
             | _ -> acc)
          | _ -> acc)

    method add_alloced_addr (tid : tid) (a : Addr.t) =
      (** Since this gets called on a alloc_result, lookup the size
          and inform data accordingly *)
      let open Alloc_data in
      let alloc_size =
        Option.value self#lookup_alloc_size ~default:(Word.zero 32) in
      let data = {min = a; max = Word.(a+alloc_size);
                  size = alloc_size} in
      {< alloced_addrs = Map.add alloced_addrs ~key:tid ~data >}

    (** When we see a alloc, do the right things with def *)
    method hook_alloc (def : Def.t) =
      let w =
        Word.((generate `Random
               lor (Word.of_int ~width:32 0xFFFF0000))
              land (Word.of_int ~width:32 0xDEADFFFF)) in
      let _,r = self#create_word w in
      match Def.rhs def with
      | Bil.Var v ->
        (* for visual consistency and keeping track of original
           alloc return values, assign this to the var too *)
        self#update (Def.lhs def) r |> fun ctxt ->
        ctxt#update v r |> fun ctxt ->
        (match Bil.Result.value r with
         | Bil.Imm a ->
           SM.put (ctxt#add_alloced_addr (Term.tid def) a)
         | _ -> SM.return ())
      | _ -> SM.return ()

    method add_freed_addr (tid : tid) (a : Addr.t) =
      let open Alloc_data in
      (* look up alloc size of this address (if it exists) in the
         alloc'ed address map *)
      let alloc_size =
        let alloc_data =
          Map.fold self#alloced_addrs ~init:None
            ~f:(fun ~key ~data acc ->
                if a = data.min then some data else acc) in
        match alloc_data with
        | Some alloc_data ->
          alloc_data.size
        | None -> Word.zero 32 in
      let free_data =
        {min = a;
         max = Word.(a+alloc_size);
         size = alloc_size} in
      {< freed_addrs = Map.add freed_addrs ~key:tid ~data:free_data >}

    method hook_free (def : Def.t) =
      match (Def.rhs def) with
      | Bil.Var v ->
        Option.(
          self#lookup v >>= fun r ->
          match Bil.Result.value r with
          | Bil.Imm a ->
            SM.put (self#add_freed_addr (Term.tid def) a) |> some
          | _ -> SM.return () |> some) |>
        Option.value ~default:(SM.return ())
      | _ -> SM.return ()

    method merge_errors ctxt = {< errors = ctxt#errors >}

    (** very important to merge current errors found! *)
    (** Can cause big troubles otherwise *)
    method! merge ctxt =
      let self = super#merge ctxt in
      (*printf "Size of errors: %d\n" @@ List.length self#errors;*)
      self#merge_errors ctxt
  end

(** Resolve memory of addr *)
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
  fun var -> Option.some_if (Target.CPU.is_sp var || var = ARM.CPU.r11)
      mem_start

let detect used_addr alloced_addr options =
  let open Alloc_data in
  match options.precision with
  | 1 -> Addr.(used_addr = alloced_addr.min)
  | _ -> Addr.(alloced_addr.min <= used_addr && used_addr <= alloced_addr.max)

class ['a] main ?deterministic ?random_seed ?reg_policy ?mem_policy
    proj options =
  let memory = memory_lookup proj in
  let lookup = register_lookup proj in
  object(self)
    constraint 'a = #context
    inherit ['a] Debugger.conqueror_concretizer_debugger
        ?deterministic ?random_seed ?reg_policy
        ?mem_policy ~memory ~lookup proj as super

    method build_error addr free_tid (ctxt : context) =
      let use_tid = List.hd_exn ctxt#trace in
      let alloc_tid =
        Map.fold ctxt#alloced_addrs ~init:None
          ~f:(fun ~key ~data acc ->
              if detect addr data options then some key else acc) in
      match alloc_tid with
      | Some alloc_tid ->
        let error = {trace = ctxt#trace; use_tid; free_tid; alloc_tid; addr} in
        (*printf "%a\n%!" Uaf_error.pp error;*)
        Some error
      | None -> None

    method check_uaf ctxt addr =
      Map.fold ctxt#freed_addrs ~init:None ~f:(fun ~key ~data acc ->
          if detect addr data options
          then self#build_error addr key ctxt
          else None)

    (** Load has no side effects on storage, so no need to extract mem
        from result r *)
    method! load s a =
      super#load s a >>= fun r ->
      SM.get () >>= fun ctxt ->
      match self#check_uaf ctxt a with
      | Some error ->
        SM.put (ctxt#add_error error) >>= fun ctxt ->
        SM.get () >>= fun ctxt ->
        SM.put (ctxt#set_next None) >>= fun () ->
        SM.return r
      | None -> SM.return r

    (** The store side effect, r, is only updated in ctxt after this
        callback completes. So if we dump context without considering r, we
        won't see the most recent update to memory : dump context
        for this address will be "None".

        Therefore, we call dump_storage after adding the address, with
        the Bil.Result.*)
    method! store s a w =
      super#store s a w >>= fun r ->
      SM.get () >>= fun ctxt ->
      match self#check_uaf ctxt a with
      | Some error ->
        SM.put (ctxt#add_error error) >>= fun ctxt ->
        SM.get () >>= fun ctxt ->
        SM.put (ctxt#set_next None) >>= fun () ->
        SM.return r
      | None -> SM.return r

    method! eval_def def =
      super#eval_def def >>= fun () ->
      match Var.name (Def.lhs def) with
      | s when is_alloc_result s ->
        SM.get () >>= fun ctxt ->
        ctxt#hook_alloc def >>= fun () ->
        SM.get () >>= fun ctxt ->
        SM.return ()
      | s when is_free s
        -> SM.get () >>= fun ctxt ->
        ctxt#hook_free def >>= fun () ->
        SM.return ()
      | _ -> SM.return ()
  end

let sub_from_tid p tid =
  match Program.lookup sub_t p tid with
  | Some sub -> sub
  | None -> failwith "Entry point not found"

let uaf_run
    ?dir
    ?directives
    ~max_steps
    ~max_loop
    ~deterministic
    ?random_seed
    ~reg_policy
    ~mem_policy
    proj sub options =
  let prog = Project.program proj in
  let ctxt = new context ?dir ?directives ~max_steps ~max_loop prog in
  let biri = new main ~deterministic ?random_seed ~reg_policy
    ~mem_policy proj options in
  let res = biri#eval_sub sub in
  SM.exec res ctxt

let rec tag_errors options errors proj =
  let rec aux errors proj =
    match errors with
    | [] -> proj
    | error :: tl ->
      printf "=-=-=-=-=-=-=-= UAF! -=-=-=-=-=-=-=-\n";
      printf "%a@.@." Uaf_error.pp error;
      Uaf_error.print_callstack proj options error;
      Uaf_error.print_trace options error;
      aux tl (Visual.tag_visited proj error) in
  aux (List.rev errors) proj

let errors_to_ida proj sub errors =
  let do_ida = true in
  let filename = sprintf "test-%s.py" @@ Sub.name sub in
  if do_ida then Toida.output_all filename errors proj

let run_sub proj sub options =
  let open Flag in
  (*let directives = [] in*)
  let directives =
    [Flag (`Myself,[`Enter_term]);
     (*Flag (`Freed_addrs,[`Enter_term]);
       Flag (`Alloced_addrs,[`Enter_term]);
       Flag (`Memory,[`Enter_term]);
       Flag (`Regs,[`Enter_term]);
       Flag (`Dot,[`Enter_term]);
       Flag (`Myself,[`Enter_term; `Load]);
       Flag (`Checkpoints,[`Enter_term]);*)
    ] in
  uaf_run
    ?random_seed:None
    (*~dir:"viewer"*) (* Optional: output debugger output to dir *)
    ~directives
    ~max_steps:1_000_000
    ~max_loop:10
    ~deterministic:false
    ~reg_policy:`Random
    ~mem_policy:`Random
    proj sub options |> fun ctxt ->
  (*if options.verbose then
    (ctxt#dump_alloced_addrs;
     ctxt#dump_freed_addrs);*)
  errors_to_ida proj sub ctxt#errors;
  ctxt

let one_sub proj fname options =
  let tid = Tid.from_string_exn ("@"^fname) in
  let prog = Project.program proj in
  let sub = sub_from_tid prog tid in
  let ctxt = run_sub proj sub options in
  tag_errors options ctxt#errors proj

let all_subs proj options =
  let prog = Project.program proj in
  Term.enum sub_t prog |> Seq.fold ~init:proj ~f:(fun proj sub ->
      let ctxt = run_sub proj sub options in
      tag_errors options ctxt#errors proj)

let main proj =
  let options = Cmdline.parse argv in
  match options.fname with
  | Some fname -> one_sub proj fname options
  | None -> all_subs proj options

let () = Project.register_pass ~deps:["callsites"] main
