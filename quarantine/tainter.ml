open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

open Option.Monad_infix

(* this designates an argument of the call specified by the value *)
let call = Value.Tag.register
    ~name:"call"
    ~uuid:"056B14C3-A185-41A8-A9C1-D36ADDEDCD0A"
    (module Tid)

let seed_with s t = Term.set_attr t Taint.reg s
let self_seed t = seed_with (Term.tid t) t

let arg_matches intent v cs arg =
  let free = (Arg.rhs arg |> Exp.free_vars) in
  let free = Set.add free (Arg.lhs arg) in
  List.exists cs ~f:(function
      | Constr.Var (v',var) ->
        intent_matches arg intent &&
        V.(v = v') &&
        Set.mem free var
      | _ -> false)

let def_of_arg arg =
  let x = Bil.var (Arg.lhs arg) in
  match Arg.rhs arg with
  | Bil.Var var -> Some (Def.create var x)
  | Bil.Load (Bil.Var m as mem,a,e,s) ->
    Some (Def.create m (Bil.store ~mem ~addr:a x e s))
  | _ -> None

let use_of_arg arg =
  let x = Arg.lhs arg in
  let e = Arg.rhs arg in
  Some (Def.create x e)

let sat_pred constr term v var =
  List.for_all constr ~f:(function
      | Constr.Fun (id,v') ->
        V.(v = v') ==> Predicate.test id term var
      | _ -> true)

let exists_sat_pred constr term v vars =
  Set.exists vars ~f:(fun var -> sat_pred constr term v var)

let arg_free_vars arg = Arg.rhs arg |> Exp.free_vars

let sat_constrs constr arg v =
  let vars = arg_free_vars arg in
  Set.exists vars ~f:(fun var ->
      List.for_all constr ~f:(function
          | Constr.Fun (id,v') ->
            V.(v = v') ==> Predicate.test id arg var
          | Constr.Var (v',var') ->
            V.(v = v') ==> Var.(var = var')
          | Constr.Dep _ -> true))

let tag_by_sort intent s def =
  if intent = In
  then Term.set_attr def call
  else
    let taint_t = if s = S.Ptr then Taint.ptr else Taint.reg in
    Term.set_attr def taint_t 

let defs_of_args intent vs defn args =
  let args = Seq.to_array args in
  let cons = Defn.constrs defn in
  let sort = List.Assoc.find ~equal:V.equal (Defn.vars defn) in
  let make_def = if intent = In then use_of_arg else def_of_arg in
  let nth_arg i = try Some (Array.nget args i) with _ -> None in
  List.filter_mapi vs ~f:(fun n v ->
      nth_arg (n - 1) >>= fun arg ->
      require (v > 0) >>= fun () ->
      require (intent_matches arg intent) >>= fun () ->
      require (sat_constrs cons arg v) >>= fun () ->
      sort v >>= fun s ->
      make_def arg >>| fun def ->
      tag_by_sort intent s def)

let already_seeded blk var =
  Term.enum def_t blk |> Seq.exists ~f:(fun def ->
      Var.equal (Def.lhs def) var &&
      (Term.has_attr def Taint.reg ||
       Term.has_attr def Taint.ptr ||
       Term.has_attr def call))

let add_def intent blk def =
  if already_seeded blk (Def.lhs def)
  then blk
  else if intent = Out
  then Term.prepend def_t blk def
  else Term.append  def_t blk def

let nullify_if xs ~f =
  List.map xs ~f:(fun x -> if f x then 0 else x)

let nullify_inputs ivars = nullify_if ~f:(Set.mem ivars)
let nullify_outputs ivars =
  nullify_if ~f:(Fn.non (Set.mem ivars))

(* here goes a small name clash, that can confuse - [out] arguments
   define input variables (i.e., those variables that are bound to
   values defined by a function), and [in] arguments define output
   variables (i.e., those variables that are bound to values used by
   a function) *)
let seed_jmp prog jmp defn sub pat =
  let seed_call intent target id vs sub =
    call_of_jmp jmp  >>= fun caller ->
    require (call_matches caller id) >>= fun () ->
    callee caller prog >>= fun callee ->
    target caller sub  >>| fun blk ->
    Term.enum arg_t callee |>
    defs_of_args intent vs defn |>
    List.map ~f:(fun f -> f (Term.tid jmp)) |>
    List.fold ~init:blk ~f:(add_def intent) |>
    Term.update blk_t sub in
  let ivars = Defn.ivars defn in
  let defs id vs  =
    let vs = nullify_outputs ivars vs in
    seed_call Out return id vs in
  let uses id vs =
    let vs = nullify_inputs ivars vs in
    let self _ sub =
      Program.parent jmp_t prog (Term.tid jmp) >>= fun blk ->
      Term.find blk_t sub (Term.tid blk) in
    seed_call In self id vs in
  match pat with
  | Pat.Call (id,v,vs) ->
    let seed f s =
      Option.value ~default:sub (f id (v::vs) s) in
    seed defs sub |> seed uses
  | _ -> sub

let seed_def def defn blk pat =
  let vars = Defn.ivars defn in
  let cons = Defn.constrs defn in
  let hit v = Set.mem vars v  in
  let free = Def.free_vars def in
  let pred v = exists_sat_pred cons def v free in
  let def = match pat with
    | Pat.Move (v1,v2)
    | Pat.Load (v1,v2)
    | Pat.Store (v1,v2)
      when pred v1 && pred v2 && (hit v1 || hit v2) ->
      Some (self_seed def)
    | _ -> None in
  match def with
  | None -> blk
  | Some def -> Term.update def_t blk def


let fold_patts spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      Defn.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let patts = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold patts ~init ~f:(fun init rule -> f defn init rule)))

let seed_sub (spec : spec) prog sub =
  let defns = Spec.defns spec in
  let sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:sub ~f:(fun sub blk ->
        let blk = Term.find_exn blk_t sub (Term.tid blk) in
        Term.enum def_t blk |>
        Seq.fold ~init:blk ~f:(fun blk def ->
            fold_patts defns ~init:blk ~f:(seed_def def)) |>
        Term.update blk_t sub) in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts defns ~init:sub ~f:(seed_jmp prog jmp)))

let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)



type 'b folder = {app : 'a. 'b -> 'a term -> 'b}

let fold cls t ~init ~f = Term.enum cls t |> Seq.fold ~init ~f

let fold_terms prog ~init {app=f} =
  fold sub_t prog ~init ~f:(fun init sub ->
      fold arg_t sub ~init ~f |> fun init ->
      fold blk_t sub ~init ~f:(fun init blk ->
          fold phi_t blk ~init ~f |> fun init ->
          fold def_t blk ~init ~f |> fun init ->
          fold jmp_t blk ~init ~f))


type t = {
  ptrs : Taint.map Tid.Map.t;
  regs : Taint.map Tid.Map.t;
  sptr : Var.Set.t Tid.Map.t;
  sreg : Var.Set.t Tid.Map.t;
}

let init = {
  ptrs = Tid.Map.empty;
  regs = Tid.Map.empty;
  sptr = Tid.Map.empty;
  sreg = Tid.Map.empty;
}

let update_map term taint map =
  match Term.get_attr term taint with
  | None -> map
  | Some ptrs ->
    let tid = match Term.get_attr term call with
      | None -> Term.tid term
      | Some t -> t in
    Map.change map tid (function
      | None -> Some ptrs
      | Some ptrs' -> Some (Taint.merge ptrs ptrs'))

(* we need to mark seeded args somehow, so that we can
   seed arguments with different seeds.*)
let update_seed prog term taint map =
  let tid = Term.tid term in
  match Term.get_attr term taint with
  | None -> map
  | Some seed -> match Program.lookup def_t prog tid with
    | None -> map
    | Some def ->
      let var = Def.lhs def in
      Map.change map seed (function
          | None -> Some (Var.Set.singleton var)
          | Some vars -> Some (Set.add vars var))

let reap prog =
  let update_seed t = update_seed prog t in
  fold_terms prog ~init {app = fun t term ->
      let ptrs = update_map term Taint.ptrs t.ptrs in
      let regs = update_map term Taint.regs t.regs in
      let sptr = update_seed term Taint.ptr t.sptr in
      let sreg = update_seed term Taint.reg t.sreg in
      {ptrs; regs; sptr; sreg}
    }


let taint_of_var map t tid var =
  match Map.find map tid with
  | None -> Tid.Set.empty
  | Some vars -> match Map.find vars var with
    | None -> Tid.Set.empty
    | Some taints -> taints

let ptrs_of_var t = taint_of_var t.ptrs t
let regs_of_var t = taint_of_var t.regs t

let seed_of_var map t tid var =
  Map.find map tid >>= fun vars ->
  Option.some_if (Set.mem vars var) tid

let ptr_seed_of_var t = seed_of_var t.sptr t
let reg_seed_of_var t = seed_of_var t.sreg t
