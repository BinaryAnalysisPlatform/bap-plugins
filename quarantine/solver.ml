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

let inputs_of_rule rule =
  Rule.constr rule |>
  List.filter_map  ~f:input_of_constr |>
  V.Set.of_list

let inputs_of_rules rules =
  List.map rules ~f:inputs_of_rule |>
  List.fold ~init:V.Set.empty ~f:Set.union

let inputs_of_judgement jud =
  Set.union
    (inputs_of_rules (Judgement.conclusion jud))
    (inputs_of_rules (Judgement.premises jud))

let collect_inputs spec =
  List.fold spec ~init:[] ~f:(fun init def ->
      List.fold (Definition.judgements def) ~init ~f:(fun init jud ->
          init))

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

let seed_jmp jmp vars sub rule =
  let seed_call id es cons = match Jmp.kind jmp with
    | Ret _ | Int _ | Goto _ -> sub
    | Call call when not (our_target id (Call.target call)) -> sub
    | Call call ->
      List.filter es ~f:(function
          | E.Ptr {Ptr.base=b; off=d} ->
            Set.mem vars b || Set.mem vars d
          | E.Reg v -> Set.mem vars v) |> function
      | [] -> sub
      | es -> match Call.return call with
        | None | Some (Indirect _) -> sub
        | Some (Direct ret) -> match Term.find blk_t sub ret with
          | None -> sub
          | Some blk ->
            List.fold es ~init:blk ~f:(fun blk -> function
                | E.Ptr {Ptr.base=b; off=o} ->
                  insert_seeds vars cons b blk |>
                  insert_seeds vars cons o
                | E.Reg v -> insert_seeds vars cons v blk) |>
            Term.update blk_t sub in
  let cons = Rule.constr rule in
  match Rule.pat rule with
  | Pat.Call (id,e1,[]) -> sub
  | Pat.Call (id,_,es) -> seed_call id es cons
  | _ -> sub

let seed_def def vars blk rule =
  let hit = Set.mem vars in
  let hitp {Ptr.base=v1; off=v2} = hit v1 || hit v2 in
  let seed = Some (self_seed def) in
  let def = match Rule.pat rule with
    | Pat.Move (v1,v2) when hit v1 || hit v2 -> seed
    | Pat.Load (v,p)
    | Pat.Store (v,p) when hit v || hitp p -> seed
    | _ -> None in
  match def with
  | None -> blk
  | Some def -> Term.update def_t blk def

let fold_judgements spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      Definition.judgements defn |>
      List.fold ~init ~f:(fun init jud ->
          let vars = inputs_of_judgement jud in
          let rules = Judgement.premises jud @
                      Judgement.conclusion jud in
          List.fold rules ~init ~f:(fun init rule -> f vars init rule)))

let seed_sub (spec : t) sub =
  let sub = Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |> Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_judgements spec ~init:sub ~f:(fun vars sub rule ->
              seed_jmp jmp vars sub rule))) in
  Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum def_t blk |> Seq.fold ~init:blk ~f:(fun blk def ->
          fold_judgements spec ~init:blk ~f:(seed_def def)) |>
      Term.update blk_t sub)


let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec)


(* three rule sets shouldn't intersect.  If a user provided the same
   rule under premise and and conclusion, then shame on him. *)
type hyp = {
  prems   : Rule.Set.t;
  concs   : Rule.Set.t;
  proofs  : tid Rule.Map.t;
  ivars   : Var.Set.t;
  ovars   : Var.Set.t;
  satdep  : v -> var -> bool;
}

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

let sat_def v def constr =
  let open Constr in
  List.for_all constr ~f:(function
      | Dep _ | Int _ -> true
      | Fun _ -> false
      | Var (v',var) -> V.(v' = v) ==> Var.(def = var))

let sat_use v use constr satdep =
  let open Constr in
  List.for_all constr ~f:(function
      | Fun _ -> false | Int _ -> true
      | Dep (v1,v2)  -> V.(v = v1) ==> satdep v2 use
      | Var (v',var) -> V.(v = v') ==> Var.(use = var))

let match_call id ins outs constr hyp = [hyp]

let match_move t src dst constr hyp sat unsat  =
  let def = Def.lhs t in
  let _use = Def.free_vars t in
  if sat_def src def constr
  then []
  else []


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

let run rs f t =
  Seq.of_list rs |> iterm ~f:(fun r ->
      SM.get () >>= fun s ->
      SM.put {s with hyps = []} >>= fun () ->
      Seq.of_list s.hyps |> iterm ~f:(fun h ->
          SM.get () >>= fun s ->
          let hs = f r t h in
          SM.put {s with hyps = List.rev_append hs s.hyps }))

class type checker = object
  method arg : arg term -> hyp -> hyp list
  method phi : phi term -> hyp -> hyp list
  method def : def term -> hyp -> hyp list
  method jmp : jmp term -> hyp -> hyp list
end

class solver (rs : checker list) =
  object
    method arg = run rs (fun r -> r#arg)
    method phi = run rs (fun r -> r#phi)
    method def = run rs (fun r -> r#def)
    method jmp = run rs (fun r -> r#jmp)
  end

class rule_base : checker = object
  method arg _ = List.return
  method phi _ = List.return
  method def _ = List.return
  method jmp _ = List.return
end
