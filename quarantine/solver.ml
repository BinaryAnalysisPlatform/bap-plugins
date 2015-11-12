open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec
open Format

module SM = Monad.State
open SM.Monad_infix

type t = spec

type matches =
  | Sat_def of v * var
  | Sat_use of v * var
  | Sat_any of matches list
  | Sat_all of matches list
with variants

let sat_bot = sat_any []
let sat_top = sat_all []

exception Unbound_predicate of string with sexp

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
  cvars   : V.Set.t;
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

let seeded_arg = Value.Tag.register
    ~name:"seeded_arg"
    ~uuid:"657af506-69c7-4534-98db-c33f160aa063"
    (module Arg)

let sub_regs = Value.Tag.register
    ~name:"sub_tainted_regs"
    ~uuid:"e3b65d77-3a2b-44a8-81c4-76c963d8e81f"
    (module Taint.Map)

let sub_ptrs = Value.Tag.register
    ~name:"sub_tainted_ptrs"
    ~uuid:"5f2a0ec1-32e4-4c68-a74e-78c5604603e7"
    (module Taint.Map)

let rec pp_matches ppf = function
  | Sat_all [] -> fprintf ppf "T@;"
  | Sat_any [] -> fprintf ppf "F@;"
  | Sat_def (v,var) -> fprintf ppf "%a <- %a@;" V.pp v Var.pp var
  | Sat_use (v,var) -> fprintf ppf "%a -> %a@;" V.pp v Var.pp var
  | Sat_all constrs -> fprintf ppf "%a@;" (pp_terms "/\\") constrs
  | Sat_any constrs -> fprintf ppf "(%a)@;" (pp_terms "\\/") constrs
and pp_terms sep ppf = function
  | [] -> ()
  | [c] -> pp_matches ppf c
  | c :: cs ->
    fprintf ppf "%a%s@;%a" pp_matches c sep (pp_terms sep) cs


let update f = SM.get () >>= fun s -> SM.put (f s)

let iterm ~f =
  Seq.fold ~init:(SM.return ()) ~f:(fun m v ->
      m >>= fun () -> f v)

type 'b folder = {app : 'a. 'b -> 'a term -> 'b}

let fold cls t ~init ~f = Term.enum cls t |> Seq.fold ~init ~f

let fold_terms prog ~init {app=f} =
  fold sub_t prog ~init ~f:(fun init sub ->
      fold arg_t sub ~init ~f |> fun init ->
      fold blk_t sub ~init ~f:(fun init blk ->
          fold phi_t blk ~init ~f |> fun init ->
          fold def_t blk ~init ~f |> fun init ->
          fold jmp_t blk ~init ~f))

let foreach cls t ~f = Term.enum cls t |> iterm ~f

let search prog (vis : 'a vis) =
  foreach sub_t prog ~f:(fun sub ->
      foreach arg_t sub ~f:vis#arg >>= fun () ->
      foreach blk_t sub ~f:(fun blk ->
          foreach phi_t blk ~f:vis#phi >>= fun () ->
          foreach def_t blk ~f:vis#def >>= fun () ->
          foreach jmp_t blk ~f:vis#jmp))


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

let seed_with s t =
  (* eprintf "Seeding %s@." (Term.name t); *)
  Term.set_attr t Taint.reg s
let self_seed t = seed_with (Term.tid t) t

let callee prog call = match Call.target call with
  | Indirect _ -> None
  | Direct tid -> Term.find sub_t prog tid

let intent_matches x y = match x with
  | None -> true
  | Some x -> match x,y with
    | In,In | Out,Out -> true
    | Both,_| _,Both -> true
    | _ -> false

let arg_matches intent v cs arg =
  let free = (Arg.rhs arg |> Exp.free_vars) in
  let free = Set.add free (Arg.lhs arg) in
  List.exists cs ~f:(function
      | Constr.Var (v',var) ->
        intent_matches (Arg.intent arg) intent &&
        V.(v = v') &&
        Set.mem free var
      | _ -> false)

let def_of_arg intent v cs arg =
  let x = Bil.var (Arg.lhs arg) in
  if intent_matches (Arg.intent arg) intent
  then match Arg.rhs arg with
    | Bil.Var var -> Some (Def.create var x)
    | Bil.Load (Bil.Var m as mem,a,e,s) ->
      Some (Def.create m (Bil.store ~mem ~addr:a x e s))
    | _ -> None
  else None

let defs_of_args intent v cs args =
  Seq.filter_map args ~f:(def_of_arg intent v cs)

let caller id jmp = match Jmp.kind jmp with
  | Ret _ | Int _ | Goto _ -> None
  | Call call when not (our_target id (Call.target call)) -> None
  | Call call -> Some call

let return sub caller =
  match Call.return caller with
  | None | Some (Indirect _) -> None
  | Some (Direct tid) -> Term.find blk_t sub tid

let tag_arg_def jmp call term =
  seed_with (Term.tid jmp) term

let already_seeded blk var =
  Term.enum def_t blk |> Seq.exists ~f:(fun def ->
      Set.mem (Def.free_vars def) var &&
      Term.has_attr def Taint.reg)

let prepend_def blk def =
  if already_seeded blk (Def.lhs def)
  then blk
  else Term.prepend def_t blk def

let test_pred name term var =
  let open Predicate in match lookup name with
  | None -> raise (Unbound_predicate name)
  | Some {sat} -> sat term var

let sat_pred constr term v vars =
  Set.exists vars ~f:(fun var ->
      List.for_all constr ~f:(function
          | Constr.Fun (id,v') ->
            V.(v = v') ==> test_pred id term var
          | _ -> true))

let seed_jmp prog jmp cons vars sub pat =
  let open Option.Monad_infix in
  let seed_call intent f id e =
    caller id jmp      >>= fun caller ->
    callee prog caller >>= fun callee ->
    return sub caller  >>| fun return ->
    Term.enum arg_t callee |>
    defs_of_args intent e cons |>
    Seq.map ~f:(tag_arg_def jmp caller) |>
    Seq.fold ~init:return ~f |>
    Term.update blk_t sub in
  let seed_call = seed_call Out prepend_def in
  match pat with
  | Pat.Call (id,0,_) -> sub
  | Pat.Call (id,e,_) when Set.mem vars e ->
    Option.value ~default:sub (seed_call id e)
  | _ -> sub

let seed_def def cons vars blk pat =
  let hit v = Set.mem vars v  in
  let free = Def.free_vars def in
  let pred v = sat_pred cons def v free in
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
      let vars = inputs_of_defn defn in
      let cons = Defn.constrs defn in
      Defn.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let rules = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold rules ~init ~f:(fun init rule -> f cons vars init rule)))

let seed_sub (spec : t) prog sub =
  let sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:sub ~f:(fun sub blk ->
        Term.enum def_t blk |>
        Seq.fold ~init:blk ~f:(fun blk def ->
            fold_patts spec ~init:blk ~f:(seed_def def)) |>
        Term.update blk_t sub) in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts spec ~init:sub ~f:(seed_jmp prog jmp)))

let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)


let taint_of_sort = function
  | S.Reg -> Taint.regs
  | S.Ptr -> Taint.ptrs



let sat term hyp kind v bil : hyp option =
  let open Option.Monad_infix in
  let dep_use y x =
    List.Assoc.find (Defn.vars hyp.defn) v >>|
    taint_of_sort >>=
    Term.get_attr term >>= fun vars ->
    Map.find vars y >>= function
    | ss when Set.is_empty ss -> None
    | ss -> match Map.find_exn hyp.ivars x with
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
    (* Term.get_attr term Taint.reg >>= fun seed -> *)
    let seed = Term.tid term in
    match Map.find_exn hyp.ivars x with
    | Top -> Some {
        hyp with
        ivars = Map.add hyp.ivars ~key:x
            ~data:(Set (Tid.Set.singleton seed))
      }
    | Set seeds ->
      if Set.mem seeds seed then (Some hyp) else None in
  let open Constr in
  List.fold hyp.constrs ~init:(Some hyp) ~f:(fun hyp cs ->
      hyp >>= fun hyp ->
      let sat c = Option.some_if c hyp in
      match cs with
      | Fun (id,v') -> V.(v = v') ==> test_pred id term bil |> sat
      | Var (v',ex) -> V.(v' = v) ==> Var.(ex = bil) |> sat
      | Dep (v1,v2) ->  match kind with
        | `def when V.(v2 = v) -> dep_def v2
        | `use when V.(v = v1) -> dep_use bil v2
        | _ -> sat true)

let merge_hyps xs =
  List.reduce_exn xs ~f:(fun h1 h2 -> {
        h1 with
        ivars = Map.merge h1.ivars h2.ivars ~f:(fun ~key r ->
            Option.some @@ match r with
            | `Left _ | `Right _ -> assert false
            | `Both (Set xs, Set ys) -> Set (Set.union xs ys)
            | `Both (_,Set xs)
            | `Both  (Set xs,_) -> Set xs
            | `Both (Top,Top) -> Top)
      })

let solution term hyp (eqs : matches) : hyp option =
  let rec solve hyp = function
    | Sat_all [] -> Some hyp
    | Sat_any [] -> None
    | Sat_def (v,bil) -> sat term hyp `def v bil
    | Sat_use (v,bil) -> sat term hyp `use v bil
    | Sat_all constrs -> forall hyp constrs
    | Sat_any constrs -> exists hyp constrs
  and forall hyp constrs =
    List.map constrs ~f:(solve hyp) |>
    Option.all |> function
    | None -> None
    | Some hyps -> Some (merge_hyps hyps)
  and exists hyp constrs =
    List.filter_map constrs ~f:(solve hyp) |> function
    | [] -> None
    | hyps -> Some (merge_hyps hyps) in
  (* if eqs <> Sat_any [] then *)
  (*   eprintf "%ssat %a@." *)
  (*     (if solve hyp eqs = None then "un" else "") pp_matches eqs; *)
  solve hyp eqs

let proved hyp pat term = {
  hyp with
  proofs = Map.add hyp.proofs ~key:pat ~data:((Term.tid term));
}

let fold_pats field matches term hyp =
  Set.fold (Field.get field hyp) ~init:hyp ~f:(fun hyp pat ->
      if Map.mem hyp.proofs pat then hyp
      else match solution term hyp (matches pat) with
        | Some hyp -> proved hyp pat term
        | None ->
          Set.add (Field.get field hyp) pat |> Field.fset field hyp)

let decide_hyp matcher term hyp =
  fold_pats Fields_of_hyp.prems matcher term hyp |>
  fold_pats Fields_of_hyp.concs matcher term

let is_done h =
  Map.length h.proofs = Set.length h.prems + Set.length h.concs

let run mrs f t =
  let step mr h = update (fun s ->
      if is_done h then {s with hyps = h :: s.hyps}
      else
        let h = decide_hyp (f mr t) t h in
        if Map.is_empty h.proofs then s
        else {s with hyps = h :: s.hyps}) in
  Seq.of_list mrs |> iterm ~f:(fun mr ->
      SM.get () >>= fun s ->
      update (fun s -> {s with hyps = []}) >>= fun () ->
      Seq.of_list (s.init @ s.hyps) |> iterm ~f:(step mr))

module Match = struct
  open Option.Monad_infix

  let sat_any_var v vars : matches = match v with
    | 0 -> sat_top
    | v ->
      Set.fold vars ~init:[] ~f:(fun cs var ->
          sat_use v var :: cs) |>
      sat_any

  class matcher : object
    method arg : arg term -> pat -> matches
    method phi : phi term -> pat -> matches
    method def : def term -> pat -> matches
    method jmp : jmp term -> pat -> matches
  end = object
    method arg _ _ = Sat_any []
    method phi _ _ = Sat_any []
    method def _ _ = Sat_any []
    method jmp _ _ = Sat_any []
  end

  let sat_mem sat v : exp -> matches =
    let sat_vars v exp =
      sat_any_var v (Exp.free_vars exp) in
    Exp.fold ~init:sat_bot (object
      inherit [matches] Bil.visitor
      method! enter_load ~mem ~addr e s eqs =
        match v with
        | `load v -> sat_any [sat_vars v addr; eqs]
        | `store _ -> eqs
      method! enter_store ~mem ~addr ~exp e s eqs =
        match v with
        | `load _ -> eqs
        | `store (p,v) ->
          sat_any [sat_all [sat_vars p addr; sat_vars p exp]; eqs]
    end)

  let move = object
    inherit matcher
    method def t r =
      let lhs,rhs = Def.(lhs t, rhs t) in
      match r with
      | Pat.Move (dst,src) ->
        sat_all [sat_def dst lhs; sat_any_var src (Def.free_vars t)]
      | Pat.Load (dst, ptr) ->
        sat_all [sat_def dst lhs; sat_mem sat (`load ptr) rhs]
      | Pat.Store (p,v) ->  sat_mem sat (`store (p,v)) rhs
      | _ -> sat_bot
  end

  let jump = object
    inherit matcher
    method jmp t r = match r with
      | Pat.Jump (k,cv,dv) ->
        let sat () : matches =
          let conds = Exp.free_vars (Jmp.cond t) in
          let dsts = Set.diff (Jmp.free_vars t) conds in
          sat_all [sat_any_var cv conds; sat_any_var dv dsts] in
        let sat = match k, Jmp.kind t with
          | `call,Call _
          | `goto,Goto _
          | `ret,Ret _
          | `exn,Int _
          | `jmp,_     -> sat
          | _ -> fun _ -> Sat_any [] in
        sat ()
      | _ -> Sat_any []
  end

  let args_free_vars =
    Seq.fold ~init:Var.Set.empty ~f:(fun vars arg ->
        let vars = Set.union vars (Arg.rhs arg |> Exp.free_vars) in
        Set.add  vars (Arg.lhs arg))


  let sat_any_arg v args : matches =
    Seq.to_list_rev args |>
    List.concat_map ~f:(fun arg ->
        let vars =
          Set.add (Arg.rhs arg |> Exp.free_vars) (Arg.lhs arg) in
        [sat_any_var v vars]) |>
    sat_any

  let call prog =
    let with_args call f : matches =
      let args =
        callee prog call >>= fun sub ->
        let args = Term.enum arg_t sub in
        if Seq.is_empty args then None
        else Some args in
      match args with
      | None -> sat_bot
      | Some args -> f args in

    let match_call_uses call vars : matches =
      with_args call (fun args ->
          Seq.zip (Seq.of_list vars) args |>
          Seq.map ~f:(fun (v,a) ->
              sat_any [
                sat_use v (Arg.lhs a);
                sat_any_var v (Arg.rhs a |> Exp.free_vars)
              ]) |> Seq.to_list_rev |> sat_all) in

    let match_call_def call v : matches =
      with_args call (fun args ->
          Seq.filter args ~f:(fun a -> Arg.intent a = Some Out) |>
          Seq.map ~f:(fun a -> sat_def v (Arg.lhs a)) |>
          Seq.to_list |> sat_any) in

    object
      inherit matcher
      method jmp t r : matches =
        match r, Jmp.kind t with
        | Pat.Call (id,ret,args), Call c
          when our_target id (Call.target c) -> sat_all [
            match_call_def  c ret;
            match_call_uses c args
          ]
        | Pat.Move (v1,v2), Call c -> sat_all [
            match_call_def c v1;
            with_args c (sat_any_arg v2)
          ]
        | Pat.Wild v, Call c -> with_args c (sat_any_arg v)
        | _ -> sat_bot  (* TODO: add Load and Store pats *)

    end

  let wild =
    let any es pat = match pat with
      | Pat.Wild v -> sat_any_var v es
      | _ -> sat_bot in
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

(* let gather_seeds seed prog : Taint.map Tid.Map.t = *)
(*   let open Option.Monad_infix in *)
(*   let update map tid arg = *)
(*     let v  = Arg.lhs arg in *)
(*     let ts = Tid.Set.singleton (Term.tid t) in *)
(*     Map.change map (Term.tid t) (function *)
(*         | None -> Some (Var.Map.singleton v ts) *)
(*         | Some vs -> Some (Map.add vs ~key:v ~data:ts)) in *)
(*   let update_seed map t = *)
(*     Term.get_attr t seed >>= fun seed -> *)
(*     Term.get_attr t seeded_arg >>| fun arg -> *)
(*     update map t seed arg in *)
(*   fold_terms prog ~init:Tid.Map.empty { *)
(*     app = fun map t -> *)
(*       match update_seed map t with *)
(*       | None -> map *)
(*       | Some map -> map *)
(*   } *)




let nullify_pattern cvars pat =
  let f v = if Set.mem cvars v then v else V.null in
  match pat with
  | Pat.Jump  (k,x,y) -> Pat.Jump (k, f x, f y)
  | Pat.Load  (x,y) -> Pat.Load (f x, f y)
  | Pat.Store (x,y) -> Pat.Store (f x, f y)
  | Pat.Move  (x,y) -> Pat.Move (f x, f y)
  | Pat.Wild x -> Pat.Wild (f x)
  | Pat.Call (id,r,xs) ->
    Pat.Call (id,f r, List.filter xs ~f:(Set.mem cvars))

let hyp_of_rule defn constrs r =
  let ivars,cvars =
    List.fold constrs ~init:(V.Map.empty,V.Set.empty)
      ~f:(fun (ivars,cvars) cs -> match cs with
          | Constr.Dep (v1,v2) ->
            Map.add ivars ~key:v2 ~data:Top,
            Set.add (Set.add cvars v1) v2
          | Constr.Var (v,_)
          | Constr.Fun (_,v) -> (ivars,Set.add cvars v)) in
  let nullify = List.map ~f:(nullify_pattern cvars) in
  let prems = nullify (Rule.premises r) in
  let concs = nullify (Rule.conclusions r) in
  {
    defn; rule = r;
    prems = Pat.Set.of_list prems;
    concs = Pat.Set.of_list concs;
    ivars;
    cvars;
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


let decide hyps =
  List.fold hyps ~init:([],[]) ~f:(fun (p,u) h ->
      let proofs = Pat.Set.of_list (Map.keys h.proofs) in
      let proved s = Set.is_empty (Set.diff s proofs) in
      if proved h.prems && proved h.concs then h :: p, u else
      if proved h.prems && not (proved h.concs) then p, h :: u
      else p,u)

let fix_decision (sats,unsats) =
  sats,List.filter unsats ~f:(fun unsat ->
      not (List.exists sats ~f:(fun sat ->
          let ps1 = Tid.Set.of_list (Map.data sat.proofs) in
          let ps2 = Tid.Set.of_list (Map.data unsat.proofs) in
          Set.is_empty @@ Set.diff ps2 ps1)))

let line = "--------------------------------"

type 'a pp = formatter -> ('a, formatter, unit) format -> 'a

let pp_hyp (pp : 'a pp) ppf h =
  let pp_pat ppf pat = match Map.find h.proofs pat with
    | None -> fprintf ppf "%s: %a@;" "unproved" Pat.pp pat
    | Some t -> fprintf ppf "%a: %a@;" Tid.pp t Pat.pp pat in
  let pp_pats ppf pats = Set.iter pats ~f:(pp_pat ppf) in
  pp ppf "@[<v2>rule %s_%s ::=@;%a%s@;%a@]@;"
    (Defn.name h.defn) (Rule.name h.rule)
    pp_pats h.prems line pp_pats h.concs


let gather_by_rule hyps rule =
  List.filter hyps ~f:(fun h -> Rule.(h.rule = rule))

let gather_by_defn hyps defn =
  List.filter hyps ~f:(fun h -> Defn.(h.defn = defn))

let collect field init =
  List.fold ~init ~f:(fun rules h -> Set.add rules (Field.get field h))

let rules = collect Fields_of_hyp.rule Rule.Set.empty
let defns = collect Fields_of_hyp.defn Defn.Set.empty

type category = [
  | `satisfied
  | `unsatisfied
  | `unrecognized
]

let pp_solution category ppf {hyps} =
  let make_pp expected ppf =
    if category = expected then fprintf ppf else ifprintf ppf in
  let pp_unr ppf = make_pp `unrecognized ppf in
  let pp_sat ppf = make_pp `satisfied ppf in
  let pp_uns ppf = make_pp `unsatisfied ppf in
  Set.iter (defns hyps) ~f:(fun defn ->
      let hyps = gather_by_defn hyps defn in
      let defn_cat = ref `satisfied in
      Set.iter (rules hyps) ~f:(fun rule ->
          let hyps = gather_by_rule hyps rule in
          let sat,uns = fix_decision (decide hyps) in
          match sat, uns with
          | [],[] ->
            defn_cat := `unrecognized;
            pp_unr ppf "@[rule %s_%s@]@;"
              (Defn.name defn) (Rule.name rule)
          | sat,[] ->
            List.iter sat ~f:(pp_hyp pp_sat ppf)
          | sat,uns ->
            defn_cat := `unsatisfied;
            List.iter sat ~f:(pp_hyp pp_sat ppf);
            List.iter uns ~f:(pp_hyp pp_uns ppf));
      let line = "::" in
      let name = Defn.name defn in
      match defn_cat.contents with
      | `satisfied    -> pp_sat ppf "%s rule %s is satisfied@;@;" line name
      | `unsatisfied  -> pp_uns ppf "%s rule %s is unsatisfied@;@;" line name
      | `unrecognized -> pp_unr ppf "%s rule %s has no models@;@;" line name);
