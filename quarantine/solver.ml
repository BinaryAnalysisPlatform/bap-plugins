open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec
open Format

module SM = Monad.State
open SM.Monad_infix


type t = spec


let create = ident


let input_of_constr = function
  | Constr.Dep (_,v) -> Some v
  | _ -> None

let inputs_of_defn rule =
  Definition.constrs rule |>
  List.filter_map  ~f:input_of_constr |>
  V.Set.of_list


let our_target id = function
  | Indirect _ -> false
  | Direct tid -> Tid.name tid = "@"^id


let unknown_of_var msg var =
  Def.create var (Bil.unknown msg (Var.typ var))

let def_of_cons v cons =
  let var = List.find_map cons ~f:(function
      | Constr.Var (v',var) when V.(v' = v) -> Some var
      | _ -> None) in
  let int = List.find_map cons ~f:(function
      | Constr.Int (v',int) when V.(v' = v) -> Some int
      | _ -> None) in
  match var,int with
  | Some x, Some w -> Some (Def.create x (Bil.int w))
  | Some x, None -> Some (unknown_of_var "undefined after call" x)
  | _ -> None

let self_seed t = Term.set_attr t Taint.seed (Term.tid t)

let insert_seeds vars cons v blk =
  if Set.mem vars v then match def_of_cons v cons with
    | None -> blk
    | Some def -> Term.prepend def_t blk (self_seed def)
  else blk

let seed_jmp jmp cons vars sub pat =
  let seed_call id es cons = match Jmp.kind jmp with
    | Ret _ | Int _ | Goto _ -> sub
    | Call call when not (our_target id (Call.target call)) -> sub
    | Call call ->
      List.filter es ~f:(fun (E.Ptr v | E.Reg v) ->
          Set.mem vars v) |> function
      | [] -> sub
      | es -> match Call.return call with
        | None | Some (Indirect _) -> sub
        | Some (Direct ret) -> match Term.find blk_t sub ret with
          | None -> sub
          | Some blk ->
            List.fold es ~init:blk ~f:(fun blk -> fun (E.Ptr v | E.Reg v) ->
                insert_seeds vars cons v blk) |>
            Term.update blk_t sub in
  match pat with
  | Pat.Call (id,e1,[]) -> sub
  | Pat.Call (id,_,es) -> seed_call id es cons
  | _ -> sub

let seed_def def cons vars blk pat =
  let hit = Set.mem vars in
  let seed = Some (self_seed def) in
  let def = match pat with
    | Pat.Move (v1,v2)
    | Pat.Load (v1,v2)
    | Pat.Store (v1,v2) when hit v1 || hit v2 -> seed
    | _ -> None in
  match def with
  | None -> blk
  | Some def -> Term.update def_t blk def

let fold_patts spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      let vars = inputs_of_defn defn in
      let cons = Definition.constrs defn in
      Definition.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let rules = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold rules ~init ~f:(fun init rule -> f cons vars init rule)))

let seed_sub (spec : t) sub =
  let sub = Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |> Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts spec ~init:sub ~f:(seed_jmp jmp))) in
  Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum def_t blk |> Seq.fold ~init:blk ~f:(fun blk def ->
          fold_patts spec ~init:blk ~f:(seed_def def)) |>
      Term.update blk_t sub)


let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec)


type sat = [`def | `use] -> v -> var -> bool

(* three rule sets shouldn't intersect.  If a user provided the same
   rule under premise and and conclusion, then shame on him. *)
type hyp = {
  prems   : Rule.Set.t;
  concs   : Rule.Set.t;
  proofs  : tid Rule.Map.t;
  sat     : sat;
  ivars   : Var.Set.t;
  ovars   : Var.Set.t;
} with fields

class type ['a] vis = object
  method arg : arg term -> (unit,'a) SM.t
  method phi : phi term -> (unit,'a) SM.t
  method def : def term -> (unit,'a) SM.t
  method jmp : jmp term -> (unit,'a) SM.t
end

type state = {
  id : string;
  hyps : hyp list;
}

let fold_rules field matches term hyp =
  Set.fold (Field.get field hyp) ~init:hyp ~f:(fun hyp rule ->
      if matches rule hyp.sat
      then {
        hyp with
        proofs = Map.add hyp.proofs ~key:rule ~data:(Term.tid term);
      } else
        Set.add (Field.get field hyp) rule |> Field.fset field hyp)

let decide_hyp matcher term hyp =
  fold_rules Fields_of_hyp.prems matcher term hyp |>
  fold_rules Fields_of_hyp.concs matcher term



let update f = SM.get () >>= fun s -> SM.put (f s)


let iterm ~f =
  Seq.fold ~init:(SM.return ()) ~f:(fun m v ->
      m >>= fun () -> f v)

let foreach cls t ~f = Term.enum cls t |> iterm ~f

let search prog (vis : 'a vis) =
  foreach sub_t prog ~f:(fun sub ->
      foreach arg_t sub ~f:vis#arg >>= fun () ->
      foreach blk_t sub ~f:(fun blk ->
          foreach phi_t blk ~f:vis#phi >>= fun () ->
          foreach def_t blk ~f:vis#def >>= fun () ->
          foreach jmp_t blk ~f:vis#jmp))

class matcher : object
  method arg : arg term -> rule -> sat -> bool
  method phi : phi term -> rule -> sat -> bool
  method def : def term -> rule -> sat -> bool
  method jmp : jmp term -> rule -> sat -> bool
end = object
  method arg _ _ _ = false
  method phi _ _ _ = false
  method def _ _ _ = false
  method jmp _ _ _ = false
end

let run (mrs : matcher list) f t =
  Seq.of_list mrs |> iterm ~f:(fun mr ->
      SM.get () >>= fun s ->
      update (fun s -> {s with hyps = []}) >>= fun () ->
      Seq.of_list s.hyps |> iterm ~f:(fun h ->
          update (fun s ->
              {s with hyps = decide_hyp (f mr t) t h :: s.hyps})))

let solver (rs : matcher list) : state vis  = object
  method arg = run rs (fun r -> r#arg)
  method phi = run rs (fun r -> r#phi)
  method def = run rs (fun r -> r#def)
  method jmp = run rs (fun r -> r#jmp)
end

let sat_def constr v def =
  let open Constr in
  List.for_all constr ~f:(function
      | Dep _ | Int _ -> true
      | Fun _ -> false
      | Var (v',var) -> V.(v' = v) ==> Var.(def = var))

let sat_use v constr satdep use =
  let open Constr in
  List.for_all constr ~f:(function
      | Fun _ -> false | Int _ -> true
      | Dep (v1,v2)  -> V.(v = v1) ==> satdep v2 use
      | Var (v',var) -> V.(v = v') ==> Var.(use = var))

(* let matches_move t src dst constr hyp  = *)
(*   let def = Def.lhs t in *)
(*   let use = Def.free_vars t in *)
(*   sat_def src constr def || *)
(*   Set.exists use ~f:(sat_use dst constr hyp.satdep) *)

let sat_any sat v uses =
  Set.exists uses ~f:(sat `use v)

(* module Match = struct *)
(*   let move = object *)
(*     inherit matcher *)
(*     method def t r sat = match r with *)
(*       | Pat.Move (dst,src) -> *)
(*         let def = Def.lhs t in *)
(*         let use = Def.free_vars t in *)
(*         sat `def dst def || sat_any sat src use *)
(*       | _ -> false *)
(*   end *)

(*   let jump = object *)
(*     inherit matcher *)
(*     method jmp t r sat = match r with *)
(*       | Pat.Jump (k,cv,dv) -> *)
(*         let sat () = *)
(*           let conds = Exp.free_vars (Jmp.cond t) in *)
(*           let dsts = Set.diff (Jmp.free_vars t) conds in *)
(*           sat_any sat cv conds || sat_any sat dv dsts in *)
(*         let sat = match k, Jmp.kind t with *)
(*           | `call,Call _ -> sat *)
(*           | `goto,Goto _ -> sat *)
(*           | `ret,Ret _   -> sat *)
(*           | `exn,Int _  -> sat *)
(*           | `jmp,_     -> sat *)
(*           | _ -> fun _ -> false in *)
(*         sat () *)
(*       | _ -> false *)
(*   end *)

(*   let load = object *)
(*     inherit matcher *)
(*     method def t r sat = *)
(*       match r with *)
(*       | Pat.Load (dst, {Ptr.base; off}) -> *)
(*         sat `def dst (Def.lhs t) || *)
(*         Exp.fold ~init:false (object *)
(*           inherit [bool] Bil.visitor *)
(*         end) (Def.rhs t) *)
(*       | _ -> false *)
(*   end *)

(* end *)
