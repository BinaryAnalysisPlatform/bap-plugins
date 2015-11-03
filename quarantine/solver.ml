open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec
open Format

module SM = Monad.State
open SM.Monad_infix

type t = spec

type sat = [`def | `use] -> v -> var -> bool

type eq =
  | Top
  | Tid of tid

type hyp = {
  prems   : Pat.Set.t;
  concs   : Pat.Set.t;
  proofs  : tid Pat.Map.t;
  ivars   : eq V.Map.t;
  ivar    : v -> v option;
  constrs : constr list;
} with fields

type state = {
  init : hyp list;
  hyps : hyp list;
}

type solution = state

class type ['a] vis = object
  method arg : arg term -> (unit,'a) SM.t
  method phi : phi term -> (unit,'a) SM.t
  method def : def term -> (unit,'a) SM.t
  method jmp : jmp term -> (unit,'a) SM.t
end

let sat term hyp kind v bil =
  let dep_use y x =
    match Term.get_attr term Taint.vars with
    | None -> false
    | Some vars -> match Map.find vars y with
      | None -> false
      | Some seeds -> match Map.find_exn hyp.ivars x with
        | Top -> true
        | Tid seed -> Set.mem seeds seed in
  let dep_def x =
    match Term.get_attr term Taint.seed with
    | None -> false
    | Some seed ->
      match Map.find_exn hyp.ivars x with
      | Top -> true
      | Tid seed' -> Tid.(seed = seed') in
  let open Constr in
  List.for_all hyp.constrs ~f:(function
      | Fun (id,v') -> V.(v <> v')
      | Int _ -> true
      | Var (v',e) -> V.(v' = v) ==> Var.(e = bil)
      | Dep (v1,v2) ->  match kind with
        | `def -> V.(v2 = v) ==> dep_def v2
        | `use -> V.(v = v1) ==> dep_use bil v2)


let create = ident

let input_of_constr = function
  | Constr.Dep (_,v) -> Some v
  | _ -> None

let inputs_of_defn rule =
  Defn.constrs rule |>
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

let insert_seeds vars cons blk v =
  if Set.mem vars v then match def_of_cons v cons with
    | None -> blk
    | Some def -> Term.prepend def_t blk (self_seed def)
  else blk

let seed_jmp jmp cons vars sub pat =
  let seed_call id es cons = match Jmp.kind jmp with
    | Ret _ | Int _ | Goto _ -> sub
    | Call call when not (our_target id (Call.target call)) -> sub
    | Call call ->
      List.filter es ~f:(Set.mem vars) |> function
      | [] -> sub
      | es -> match Call.return call with
        | None | Some (Indirect _) -> sub
        | Some (Direct ret) -> match Term.find blk_t sub ret with
          | None -> sub
          | Some blk ->
            List.fold es ~init:blk ~f:(insert_seeds vars cons) |>
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
      let cons = Defn.constrs defn in
      Defn.rules defn |>
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

let sat_any sat v uses =
  Set.exists uses ~f:(sat `use v)

let ivars_of_pat f = function
  | Pat.Call (_,_,ys) -> List.filter ~f ys
  | Pat.Move (v,_)
  | Pat.Load (v,_)
  | Pat.Store (v,_) when f v -> [v]
  | _ -> []



(* here is the place where we can fix input variables.  if we have
   proved proposition [pat], that binds some variable that occurs on
   either side of `x/y` equation, then we should be sure, that `x` and
   `y` are now unified into concrete equality class, not the Top one.
*)

let proof hyp pat term =
  {
    hyp with
    proofs = Map.add hyp.proofs ~key:pat ~data:(Term.tid term);
  }

let fold_pats field matches term hyp =
  Set.fold (Field.get field hyp) ~init:hyp ~f:(fun hyp pat ->
      if matches pat (sat term hyp)
      then proof hyp pat term
      else
        Set.add (Field.get field hyp) pat |> Field.fset field hyp)

let decide_hyp matcher term hyp =
  fold_pats Fields_of_hyp.prems matcher term hyp |>
  fold_pats Fields_of_hyp.concs matcher term

let run mrs f t =
  Seq.of_list mrs |> iterm ~f:(fun mr ->
      SM.get () >>= fun s ->
      update (fun s -> {s with hyps = []}) >>= fun () ->
      Seq.of_list s.hyps |> iterm ~f:(fun h ->
          update (fun s ->
              {s with hyps = decide_hyp (f mr t) t h :: s.hyps})))

module Match = struct
  class matcher : object
    method arg : arg term -> pat -> sat -> bool
    method phi : phi term -> pat -> sat -> bool
    method def : def term -> pat -> sat -> bool
    method jmp : jmp term -> pat -> sat -> bool
  end = object
    method arg _ _ _ = false
    method phi _ _ _ = false
    method def _ _ _ = false
    method jmp _ _ _ = false
  end

  let sat_mem sat v =
    let sat_vars v exp = sat_any sat v (Exp.free_vars exp) in
    Exp.fold ~init:false (object
      inherit [bool] Bil.visitor
      method! enter_load ~mem ~addr e s found =
        found || match v with
        | `load v -> sat_vars v addr
        | `store _ -> false
      method! enter_store ~mem ~addr ~exp e s found =
        found || match v with
        | `load _ -> false
        | `store (p,v) -> sat_vars p addr && sat_vars p exp
    end)

  let move = object
    inherit matcher
    method def t r sat =
      let lhs,rhs = Def.(lhs t, rhs t) in
      match r with
      | Pat.Move (dst,src) ->
        sat `def dst lhs && sat_any sat src (Def.free_vars t)
      | Pat.Load (dst, ptr) ->
        sat `def dst lhs && sat_mem sat (`load ptr) rhs
      | Pat.Store (p,v) -> sat_mem sat (`store (p,v)) rhs
      | _ -> false
  end

  let jump = object
    inherit matcher
    method jmp t r sat = match r with
      | Pat.Jump (k,cv,dv) ->
        let sat () =
          let conds = Exp.free_vars (Jmp.cond t) in
          let dsts = Set.diff (Jmp.free_vars t) conds in
          sat_any sat cv conds && sat_any sat dv dsts in
        let sat = match k, Jmp.kind t with
          | `call,Call _
          | `goto,Goto _
          | `ret,Ret _
          | `exn,Int _
          | `jmp,_     -> sat
          | _ -> fun _ -> false in
        sat ()
      | _ -> false
  end

  let sat_arg intent sat args v =
    Seq.exists args ~f:(fun arg -> match intent, Arg.intent arg with
        | In, Some Out -> false
        | Out, Some In -> false
        | _ ->
          let lhs = if intent = In then `use else `def in
          sat lhs v (Arg.lhs arg) ||
          sat_any sat v (Arg.rhs arg |> Exp.free_vars))

  let call prog = object
    inherit matcher
    method jmp t r sat =
      let with_args call f =
        match Call.target call with
        | Indirect _ -> false
        | Direct tid -> match Term.find sub_t prog tid with
          | None -> false
          | Some sub -> f (Term.enum arg_t sub) in
      let match_call call uses defs =
        with_args call (fun args ->
            List.for_all defs ~f:(sat_arg Out sat args) &&
            List.for_all uses ~f:(sat_arg In  sat args)) in
      let match_move call v1 v2 =
        with_args call (fun args ->
            sat_arg Out sat args v1 && sat_arg In sat args v2) in
      let match_wild call v =
        with_args call (fun args -> sat_arg In sat args v) in
      match r, Jmp.kind t with
      | Pat.Call (id,uses,defs), Call c ->
        our_target id (Call.target c) && match_call c uses defs
      | Pat.Move (v1,v2), Call c -> match_move c v1 v2
      | Pat.Wild v, Call c -> match_wild c v
      | _ -> false  (* TODO: add Load and Store pats *)
  end

  let wild =
    let any es pat sat = match pat with
      | Pat.Wild v -> sat_any sat v es
      | _ -> false in
    object
      inherit matcher
      method def t = any (Def.free_vars t)
      method jmp t = any (Jmp.free_vars t)
      method arg t = any (Exp.free_vars (Arg.rhs t))
      method phi t = any (Phi.free_vars t)
    end

  let rules prog = [wild;jump;move;call prog]

end

let solver prog : state vis =
  let rs = Match.rules prog in
  object
    method arg = run rs (fun r -> r#arg)
    method phi = run rs (fun r -> r#phi)
    method def = run rs (fun r -> r#def)
    method jmp = run rs (fun r -> r#jmp)
  end

let hyp_of_rule constrs r =
  let ivars,ovars =
    List.fold constrs ~init:(V.Map.empty,V.Map.empty)
      ~f:(fun (ivars,ovars) cs -> match cs with
          | Constr.Dep (v1,v2) ->
            Map.add ivars ~key:v2 ~data:Top,
            Map.add ovars ~key:v1 ~data:v2
          | _ -> (ivars,ovars)) in
  let ivar = Map.find ovars in
  {
    prems = Pat.Set.of_list (Rule.premises r);
    concs = Pat.Set.of_list (Rule.conclusions r);
    ivars;
    ivar;
    proofs = Pat.Map.empty;
    constrs;
  }

let hyps_of_defn d =
  let constrs = Defn.constrs d in
  List.map ~f:(hyp_of_rule constrs) (Defn.rules d)

let solve spec prog =
  let state = {init = []; hyps = []} in
  let solver = solver prog in
  SM.exec (search prog solver) state
