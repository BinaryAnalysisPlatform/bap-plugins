open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

open Format

let def_summary _ = None
let def_const = Word.zero 8


let skip () = SM.return false
let pass () = SM.return true

let summaries = String.Map.of_alist_exn [
  ]

let def_summary call = match Call.target call with
  | Indirect _ -> None
  | Direct tid ->
    Map.find summaries (Tid.name tid)

class type result = object
  method visited : Tid.Set.t
  method taints_of_term : tid -> Taint.taints Var.Map.t
end

type vars = Tid.Set.t Var.Map.t Tid.Map.t

let union_map m1 m2 ~f =
  Map.merge m1 m2 ~f:(fun ~key -> function
      | `Both (v1,v2) -> Some (f v1 v2)
      | `Left v | `Right v -> Some v)

let union_vars : vars -> vars -> vars =
  union_map ~f:(union_map ~f:Set.union)

class context p total  = object(self : 's)
  inherit Taint.context as taints
  inherit Biri.context p as super

  val k = total
  val vis : Tid.Set.t = Tid.Set.empty (* visited *)
  val cps : 's Tid.Map.t = Tid.Map.empty (* checkpoints *)
  val tvs : vars = Tid.Map.empty


  method k   = k
  method tvs = tvs
  method cps = cps
  method visited = vis

  method step =
    if k > 0
    then Some {< k = k - 1 >}
    else None

  method visit_term tid =
    {< vis = Set.add vis tid; cps = Map.remove cps tid; >}

  method checkpoint tid =
    if Set.mem vis tid then self
    else {< cps = Map.add cps ~key:tid ~data:self>}

  method backtrack : (tid * 's) option =
    match Map.min_elt cps with
    | None -> None
    | Some (tid,other) ->
      Some (tid,other#merge self tid)

  method merge runner tid =
    {<
      k   = runner#k;
      vis = Set.union vis runner#visited;
      tvs = union_vars tvs runner#tvs;
      cps = runner#cps
    >}

  method taint_var tid v r =
    let ts = taints#val_taints r in
    let tvs = Map.change tvs tid (function
        | None when Set.is_empty ts -> None
        | None -> Some (Var.Map.of_alist_exn [v, ts])
        | Some vs -> Option.some @@ Map.change vs v (function
            | None when Set.is_empty ts -> None
            | None -> Some ts
            | Some ts' -> Some (Set.union ts ts'))) in
    {< tvs = tvs >}

  method taints_of_term tid =
    Map.find tvs tid |> function
    | None -> Var.Map.empty
    | Some ts -> ts

end

class ['a] main summary memory tid_of_addr const = object(self)
  constraint 'a = #context
  inherit ['a] biri as super
  inherit ['a] Taint.propagator

  method! enter_term cls t =
    let tid = Term.tid t in
    SM.get () >>= fun c ->
    SM.put (c#visit_term tid) >>= fun () ->
    super#enter_term cls t

  method! eval_unknown _ t = self#emit t

  method! lookup v =
    super#lookup v >>= fun r ->
    SM.get () >>= fun ctxt ->
    match List.hd ctxt#trace with
    | None -> SM.return r
    | Some tid ->
      SM.put (ctxt#taint_var tid v r) >>= fun () ->
      match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> self#emit (Var.typ v)

  method! load s a =
    super#load s a >>= fun r -> match Bil.Result.value r with
    | Bil.Imm _ | Bil.Mem _ -> SM.return r
    | Bil.Bot -> match memory a with
      | None -> self#emit_const 8
      | Some w ->
        SM.get () >>= fun ctxt ->
        let ctxt,r = ctxt#create_word w in
        SM.put ctxt >>= fun () ->
        SM.return r

  method! eval_jmp jmp =
    SM.get () >>= fun ctxt ->
    match ctxt#step with
    | None -> SM.put (ctxt#set_next None)
    | Some ctxt ->
      SM.put ctxt >>= fun () ->
      super#eval_jmp jmp

  method! eval_call call =
    self#shortcut_indirect call >>= fun () ->
    self#summarize_call call

  method! eval_arg arg =
    super#eval_arg arg >>= fun () ->
    match Term.get_attr arg Taint.seed with
    | None -> SM.return ()
    | Some s -> match Arg.rhs arg with
      | Bil.Var v -> self#taint_arg s v
      | _ -> SM.return ()

  method private taint_arg s v =
    let taints = Tid.Set.singleton s in
    self#lookup v >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint_val r taints)

  method! eval_indirect exp =
    self#eval_exp exp >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> self#backtrack exp
    | Bil.Imm dst ->
      match tid_of_addr dst with
      | Some dst -> self#eval_direct dst
      | None -> self#backtrack exp

  method private backtrack exp =
    SM.get () >>= fun ctxt ->
    match ctxt#backtrack with
    | None -> super#eval_indirect exp
    | Some (next,ctxt) ->
      SM.put ctxt >>= fun () -> super#eval_direct next

  method! eval_def def =
    match Term.get_attr def Taint.seed with
    | None -> super#eval_def def
    | Some seed ->
      super#eval_def def >>= fun () ->
      SM.get () >>= fun ctxt ->
      self#lookup (Def.lhs def) >>= fun x ->
      SM.put (ctxt#taint_val x (Tid.Set.singleton seed)) >>= fun () ->
      self#update (Def.lhs def) x

  method private emit t =
    match t with
    | Type.Imm sz -> self#emit_const sz
    | Type.Mem _  -> self#emit_empty

  method private emit_const sz =
    SM.get () >>= fun ctxt ->
    let const = Word.extract_exn ~lo:0 ~hi:(sz-1) const in
    let ctxt,r = ctxt#create_word const in
    SM.put ctxt >>= fun () ->
    SM.return r

  method private emit_empty =
    SM.get () >>= fun ctxt ->
    let ctxt,r = ctxt#create_storage self#empty in
    SM.put ctxt >>= fun () ->
    SM.return r

  method private shortcut_indirect call =
    match Call.target call with
    | Direct _ -> self#call_with_restore call
    | Indirect _ -> self#return call

  method private call_with_restore call =
    match Call.return call with
    | None | Some (Indirect _) -> super#eval_call call
    | Some (Direct ret) ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#checkpoint ret) >>= fun () ->
      super#eval_call call

  method private summarize_call call =
    let create f =
      SM.get () >>= fun ctxt ->
      let ctxt, v = f ctxt in
      SM.put ctxt >>= fun () ->
      SM.return v in
    match summary call with
    | None -> super#eval_call call
    | Some summary ->
      List.fold summary ~init:(SM.return ()) ~f:(fun m (x,v) ->
          m >>= fun () -> create (fun ctxt -> match v with
              | Bil.Mem s -> ctxt#create_storage s
              | Bil.Imm w -> ctxt#create_word w
              | Bil.Bot   -> ctxt#create_undefined) >>= fun r ->
          self#update x r) >>= fun () ->
      self#return call

  method private return call = match Call.return call with
    | None -> super#eval_call call
    | Some lab -> super#eval_ret lab
end

exception Entry_point_not_found

let run_from_tid p (biri : 'a #main) tid =
  match Program.lookup sub_t p tid with
  | Some sub -> biri#eval_sub sub
  | None -> raise Entry_point_not_found

let tid_of_name str =
  match Tid.from_string ("@"^str) with
  | Ok tid -> tid
  | Error _ -> raise Entry_point_not_found

let tid_of_ident mapping = function
  | `Term tid -> tid
  | `Name str -> tid_of_name str
  | `Addr add -> match mapping add with
    | None -> raise Entry_point_not_found
    | Some tid -> tid

let run_from_point mapping p biri point =
  run_from_tid p biri (tid_of_ident mapping point)


let create_mapping prog =
  let addrs = Addr.Table.create () in
  let add t a = Hashtbl.replace addrs ~key:a ~data:(Term.tid t) in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter  ~f:(fun blk ->
          match Term.get_attr blk Disasm.block with
          | Some addr -> add blk addr
          | None -> ());
      match Term.get_attr sub subroutine_addr with
      | Some addr -> add sub addr
      | None -> ());
  Hashtbl.find addrs

let memory_lookup proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.hd |> function
  | None -> None
  | Some (mem,_) -> match Memory.get ~addr mem with
    | Ok w -> Some w
    | _ -> None

let run proj k point =
  let p = Project.program proj in
  let ctxt = new context p k in
  let mapping = create_mapping p in
  let memory = memory_lookup proj in
  let biri = new main def_summary memory mapping def_const in
  let map _ = None in
  let res = run_from_point map p biri point in
  (SM.exec res ctxt :> result)
