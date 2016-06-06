open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

open Option.Monad_infix

let seed_with s t = Term.set_attr t Taint.reg s
let self_seed t = seed_with (Term.tid t) t

let sat_pred constr term v var =
  List.for_all constr ~f:(function
      | Constr.Fun (id,v') ->
        V.(v = v') ==> Predicate.test id term var
      | _ -> true)

let tag_of_sort = function
  | S.Ptr -> Taint.ptr
  | S.Reg -> Taint.reg

let seed_def target def defn blk pat =
  let vars = Defn.ivars defn in
  let sort = List.Assoc.find ~equal:V.equal (Defn.vars defn) in
  let cons = Defn.constrs defn in
  let hit v = Set.mem vars v  in
  let sat v = sat_pred cons def v (Def.rhs def) in
  match pat with
  | Pat.Move (v1,v2) | Pat.Load (v1,v2) | Pat.Store (v1,v2)
    when sat v1 && sat v2 && (hit v1 || hit v2) ->
    Some (self_seed def)
  | Pat.Call (id,v,vs) ->
    Term.get_attr def Term.origin >>= fun origin ->
    target origin >>= fun callee ->
    require (id = "_" || id = Sub.name callee) >>= fun () ->
    let args = Seq.to_array (Term.enum arg_t callee) in
    List.find_mapi (v::vs) ~f:(fun i v ->
        let arg = Array.nget args (i-1) in
        require (hit v) >>= fun () ->
        require Var.(Def.lhs def = Arg.lhs arg) >>= fun () ->
        require (sat v) >>= fun () ->
        sort v) >>| fun sort ->
    Term.set_attr def (tag_of_sort sort) origin
  | _ -> None

let fold_patts spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      Defn.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let patts = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold patts ~init ~f:(fun init rule -> f defn init rule)))

(* creates a mapping from call tid to callee *)
let resolve_subs prog =
  let subs = Tid.Table.create () in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Hashtbl.set subs ~key:(Term.tid sub) ~data:sub);
  let targets = Tid.Table.create () in
  (object(self)
    inherit [unit] Term.visitor
    method enter_jmp jmp () = match Jmp.kind jmp with
      | Call c -> self#process_call jmp c
      | _ -> ()
    method process_call jmp call = match Call.target call with
      | Indirect _ -> ()
      | Direct tid -> match Hashtbl.find subs tid with
        | None -> ()
        | Some name ->
          Hashtbl.set targets ~key:(Term.tid jmp) ~data:name
  end)#run prog ();
  Hashtbl.find targets

let seed_sub (spec : spec) prog sub =
  let defns = Spec.defns spec in
  let subs = resolve_subs prog in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      let blk = Term.find_exn blk_t sub (Term.tid blk) in
      Term.enum def_t blk |>
      Seq.fold ~init:blk ~f:(fun blk def ->
          fold_patts defns ~init:blk ~f:(fun defn blk pat ->
              match seed_def subs def defn blk pat with
              | None -> blk
              | Some def -> Term.update def_t blk def)) |>
      Term.update blk_t sub)

let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)

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
    let tid = match Term.get_attr term Term.origin with
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
      let vars = Set.add (Def.free_vars def) (Def.lhs def) in
      Map.update map seed ~f:(function
          | None -> vars
          | Some vars' -> Set.union vars vars)

let reap prog =
  let update_seed t = update_seed prog t in
  (object inherit [t] Term.visitor as super
    method! enter_term cls term t =
      let ptrs = update_map term Taint.ptrs t.ptrs in
      let regs = update_map term Taint.regs t.regs in
      let sptr = update_seed term Taint.ptr t.sptr in
      let sreg = update_seed term Taint.reg t.sreg in
      {ptrs; regs; sptr; sreg}
  end)#run prog init

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
