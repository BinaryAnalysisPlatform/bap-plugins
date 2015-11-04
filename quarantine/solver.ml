open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec
open Format

module SM = Monad.State
open SM.Monad_infix

type t = spec

type equation =
  | Sat_def of v * var
  | Sat_use of v * Var.Set.t
with variants

type equations = equation list
type match_res = equations option

(* a lattice for dependency equality class.*)
type eq =
  | Top              (* superset *)
  | Set of Tid.Set.t (* invariant: set is not empty *)

type hyp = {
  defn    : Defn.t;
  rule    : Rule.t;
  prems   : Pat.Set.t;
  concs   : Pat.Set.t;
  proofs  : tid Pat.Map.t;
  ivars   : eq V.Map.t;
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

let pp_equation ppf = function
  | Sat_def (v,var) -> fprintf ppf "%a <- %a@." V.pp v Var.pp var
  | Sat_use (v,vars)->
    fprintf ppf "%a ->  %a@." V.pp v Var.pp_seq (Set.to_sequence vars)

let pp_equations ppf eqs =
  List.iter eqs ~f:(pp_equation ppf)

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

let sat term hyp kind v bil : hyp option =
  let dep_use y x =
    match Term.get_attr term Taint.vars with
    | None -> None
    | Some vars -> match Map.find vars y with
      | None -> None
      | Some ss when Set.is_empty ss -> None
      | Some ss -> match Map.find_exn hyp.ivars x with
        | Top -> Some {
            hyp with
            ivars = Map.add hyp.ivars ~key:x ~data:(Set ss)
          }
        | Set xs ->
          let ss = Set.inter ss xs in
          if Set.is_empty ss then None
          else Some {
              hyp with
              ivars = Map.add hyp.ivars ~key:x ~data:(Set ss)
            } in
  let dep_def x =
    match Term.get_attr term Taint.seed with
    | None -> None
    | Some seed ->
      let unified = Some {
          hyp with
          ivars = Map.add hyp.ivars ~key:x
              ~data:(Set (Tid.Set.singleton seed))
        } in
      match Map.find_exn hyp.ivars x with
      | Top -> unified
      | Set seeds ->
        if Set.mem seeds seed then unified else None in
  let open Constr in
  List.fold hyp.constrs ~init:(Some hyp) ~f:(fun hyp cs ->
      match hyp with
      | None -> None
      | Some hyp ->
        let sat c = Option.some_if c hyp in
        match cs with
        | Fun (id,v') -> sat V.(v <> v')
        | Int _ -> sat true
        | Var (v',e) -> (V.(v' = v) ==> Var.(e = bil)) |> sat
        | Dep (v1,v2) ->  match kind with
          | `def when V.(v2 = v) -> dep_def v2
          | `use when V.(v = v1) -> dep_use bil v2
          | _ -> sat true)

let solution term hyp (eqs : match_res) : hyp option =
  let open Option.Monad_infix in
  eqs >>= List.fold ~init:(Some hyp) ~f:(fun hyp eq ->
      hyp >>= fun hyp -> match eq with
      | Sat_def (v,bil) -> sat term hyp `def v bil
      | Sat_use (v,vs) ->
        Set.to_list vs |>
        List.filter_map ~f:(sat term hyp `use v) |> function
        | [] -> None
        | xs -> List.reduce_exn xs ~f:(fun h1 h2 -> {
              h1 with
              ivars = Map.merge h1.ivars h2.ivars ~f:(fun ~key r ->
                  Option.some @@ match r with
                  | `Left _ | `Right _ -> assert false
                  | `Both (Set xs, Set ys) -> Set (Set.union xs ys)
                  | `Both (_,Set xs)
                  | `Both  (Set xs,_) -> Set xs
                  | `Both (Top,Top) -> Top)
            }) |> Option.some)

let proved hyp pat term =
  printf "proposition %a is proved by term %s@."
    Pat.pp pat (Term.name term);
  {
    hyp with
    proofs = Map.add hyp.proofs ~key:pat ~data:(Term.tid term);
  }

let fold_pats field matches term hyp =
  Set.fold (Field.get field hyp) ~init:hyp ~f:(fun hyp pat ->
      match solution term hyp (matches pat) with
      | Some hyp -> proved hyp pat term
      | None ->
        Set.add (Field.get field hyp) pat |> Field.fset field hyp)

let decide_hyp matcher term hyp =
  fold_pats Fields_of_hyp.prems matcher term hyp |>
  fold_pats Fields_of_hyp.concs matcher term

let run mrs f t =
  Seq.of_list mrs |> iterm ~f:(fun mr ->
      SM.get () >>= fun s ->
      update (fun s -> {s with hyps = []}) >>= fun () ->
      Seq.of_list (s.init @ s.hyps) |> iterm ~f:(fun h ->
          update (fun s ->
              let h = decide_hyp (f mr t) t h in
              if Map.is_empty h.proofs then s
              else {s with hyps = h :: s.hyps})))

module Match = struct
  class matcher : object
    method arg : arg term -> pat -> equations option
    method phi : phi term -> pat -> equations option
    method def : def term -> pat -> equations option
    method jmp : jmp term -> pat -> equations option
  end = object
    method arg _ _ = None
    method phi _ _ = None
    method def _ _ = None
    method jmp _ _ = None
  end

  let sat_mem sat v : exp -> equations =
    let sat_vars v exp = sat_use v (Exp.free_vars exp) in
    Exp.fold ~init:[] (object
      inherit [equations] Bil.visitor
      method! enter_load ~mem ~addr e s eqs =
        match v with
        | `load v -> sat_vars v addr :: eqs
        | `store _ -> eqs
      method! enter_store ~mem ~addr ~exp e s eqs =
        match v with
        | `load _ -> eqs
        | `store (p,v) ->
          sat_vars p addr :: sat_vars p exp :: eqs
    end)

  let move = object
    inherit matcher
    method def t r =
      let lhs,rhs = Def.(lhs t, rhs t) in
      match r with
      | Pat.Move (dst,src) ->
        Some [sat_def dst lhs; sat_use src (Def.free_vars t)]
      | Pat.Load (dst, ptr) ->
        Some (sat_def dst lhs :: sat_mem sat (`load ptr) rhs)
      | Pat.Store (p,v) -> Some (sat_mem sat (`store (p,v)) rhs)
      | _ -> None
  end

  let jump = object
    inherit matcher
    method jmp t r = match r with
      | Pat.Jump (k,cv,dv) ->
        let sat () : equations option =
          let conds = Exp.free_vars (Jmp.cond t) in
          let dsts = Set.diff (Jmp.free_vars t) conds in
          Some [sat_use cv conds; sat_use dv dsts] in
        let sat = match k, Jmp.kind t with
          | `call,Call _
          | `goto,Goto _
          | `ret,Ret _
          | `exn,Int _
          | `jmp,_     -> sat
          | _ -> fun _ -> None in
        sat ()
      | _ -> None
  end

  let sat_arg intent sat args v : equations =
    Seq.to_list_rev args |>
    List.concat_map ~f:(fun arg -> match intent, Arg.intent arg with
        | In, Some Out -> []
        | Out, Some In -> []
        | _ -> [
            sat_use v (Arg.rhs arg |> Exp.free_vars);
            sat_def v (Arg.lhs arg); (* XXX: is this correct? *)
          ])

  let call prog = object
    inherit matcher
    method jmp t r : equations option =
      let with_args call f : equations option =
        match Call.target call with
        | Indirect _ -> None
        | Direct tid -> match Term.find sub_t prog tid with
          | None -> None
          | Some sub -> Some (f (Term.enum arg_t sub)) in
      let match_call call uses defs =
        with_args call (fun args ->
            List.concat_map defs ~f:(sat_arg Out sat args) @
            List.concat_map uses ~f:(sat_arg In  sat args)) in
      let match_move call v1 v2 =
        with_args call (fun args ->
            sat_arg Out sat args v1 @ sat_arg In sat args v2) in
      let match_wild call v =
        with_args call (fun args -> sat_arg In sat args v) in
      match r, Jmp.kind t with
      | Pat.Call (id,uses,defs), Call c
        when our_target id (Call.target c) ->
        match_call c uses defs
      | Pat.Move (v1,v2), Call c -> match_move c v1 v2
      | Pat.Wild v, Call c -> match_wild c v
      | _ -> None  (* TODO: add Load and Store pats *)
  end

  let wild =
    let any es pat = match pat with
      | Pat.Wild v -> Some [sat_use v es]
      | _ -> None in
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

let hyp_of_rule defn constrs r =
  let ivars,ovars =
    List.fold constrs ~init:(V.Map.empty,V.Map.empty)
      ~f:(fun (ivars,ovars) cs -> match cs with
          | Constr.Dep (v1,v2) ->
            Map.add ivars ~key:v2 ~data:Top,
            Map.add ovars ~key:v1 ~data:v2
          | _ -> (ivars,ovars)) in
  {
    defn; rule = r;
    prems = Pat.Set.of_list (Rule.premises r);
    concs = Pat.Set.of_list (Rule.conclusions r);
    ivars;
    proofs = Pat.Map.empty;
    constrs;
  }

let hyps_of_defn d =
  let constrs = Defn.constrs d in
  List.map ~f:(hyp_of_rule d constrs) (Defn.rules d)

let solve spec prog =
  let init = List.concat_map spec ~f:hyps_of_defn in
  let state = {init; hyps = []} in
  let solver = solver prog in
  SM.exec (search prog solver) state

let pp_hyp ppf h =
  let proved = Set.is_empty in
  let result =
    if proved h.prems && proved h.concs then "was proved" else
    if proved h.prems && not (proved h.concs) then "wasn't proved"
    else "wasn't recognized" in
  Format.fprintf ppf "hypothesis %s %s@;"
    (Rule.name h.rule) result


let pp_solution ppf {hyps} =
  List.iter hyps ~f:(fun hyp -> pp_hyp ppf hyp)
