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

let call_result = Value.Tag.register
    ~name:"call_result"
    ~uuid:"b3582364-c105-4dd1-a8bd-0a38e3a3b548"
    (module Call)

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

let seed_with s t = Term.set_attr t Taint.seed s
let self_seed t = seed_with (Term.tid t) t

let callee prog call = match Call.target call with
  | Indirect _ -> None
  | Direct tid -> Term.find sub_t prog tid

let arg_matches vars cs arg =
  let free = (Arg.rhs arg |> Exp.free_vars) in
  let free = Set.add free (Arg.lhs arg) in
  List.exists cs ~f:(function
      | Constr.Var (v,var) ->
        Arg.intent arg <> Some In &&
        Set.mem vars v &&
        Set.mem free var
      | _ -> false)

let def_of_arg vars cs arg =
  let x = Bil.var (Arg.lhs arg) in
  if arg_matches vars cs arg
  then match Arg.rhs arg with
    | Bil.Var var -> Some (Def.create var x)
    | Bil.Load (Bil.Var m as mem,a,e,s) ->
      Some (Def.create m (Bil.store ~mem ~addr:a x e s))
    | _ -> None
  else None

let defs_of_args vars cs args =
  Seq.filter_map args ~f:(def_of_arg vars cs)

let caller id jmp = match Jmp.kind jmp with
  | Ret _ | Int _ | Goto _ -> None
  | Call call when not (our_target id (Call.target call)) -> None
  | Call call -> Some call

let return sub caller =
  match Call.return caller with
  | None | Some (Indirect _) -> None
  | Some (Direct tid) -> Term.find blk_t sub tid

let tag_arg_def call term =
  Term.set_attr (self_seed term) call_result call

let seed_jmp prog jmp cons vars sub pat =
  let open Option.Monad_infix in
  let seed_call id vars =
    caller id jmp      >>= fun caller ->
    callee prog caller >>= fun callee ->
    return sub caller  >>| fun return ->
    Term.enum arg_t callee |>
    defs_of_args vars cons |>
    Seq.map ~f:(tag_arg_def caller) |>
    Seq.fold ~init:return ~f:(Term.prepend def_t) |>
    Term.update blk_t sub in
  match pat with
  | Pat.Call (id,None,_) -> sub
  | Pat.Call (id,Some e,_) when Set.mem vars e ->
    Option.value ~default:sub (seed_call id vars)
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

let seed_sub (spec : t) prog sub =
  let sub = Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum def_t blk |> Seq.fold ~init:blk ~f:(fun blk def ->
          fold_patts spec ~init:blk ~f:(seed_def def)) |>
      Term.update blk_t sub) in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts spec ~init:sub ~f:(seed_jmp prog jmp)))

let seed spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)

let search prog (vis : 'a vis) =
  foreach sub_t prog ~f:(fun sub ->
      foreach arg_t sub ~f:vis#arg >>= fun () ->
      foreach blk_t sub ~f:(fun blk ->
          foreach phi_t blk ~f:vis#phi >>= fun () ->
          foreach def_t blk ~f:vis#def >>= fun () ->
          foreach jmp_t blk ~f:vis#jmp))

let sat term hyp kind v bil : hyp option =
  let open Option.Monad_infix in
  let dep_use y x =
    Term.get_attr term Taint.vars >>= fun vars ->
    Map.find vars y >>= function
    | ss when Set.is_empty ss -> None
    | ss ->
      match Map.find_exn hyp.ivars x with
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
    Term.get_attr term Taint.seed >>= fun seed ->
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
      hyp >>= fun hyp ->
      let sat c = Option.some_if c hyp in
      match cs with
      | Fun (id,v') -> sat V.(v <> v')
      | Var (v',e) -> (V.(v' = v) ==> Var.(e = bil)) |> sat
      | Dep (v1,v2) ->  match kind with
        | `def when V.(v2 = v) -> dep_def v2
        | `use when V.(v = v1) -> dep_use bil v2
        | _ -> sat true)

let solution term hyp (eqs : match_res) : hyp option =
  let open Option.Monad_infix in
  eqs >>= List.fold ~init:(Some hyp) ~f:(fun hyp eq ->
      hyp >>= fun hyp -> match eq with
      | Sat_def (v,_) | Sat_use (v,_)
        when not (Set.mem hyp.cvars v) -> Some hyp
      | Sat_def (v,bil) -> sat term hyp `def v bil
      | Sat_use (v,vs) ->
        Set.to_list vs |>
        List.filter_map ~f:(sat term hyp `use v) |> function
        | [] -> None
        | xs ->
          Option.some @@ List.reduce_exn xs ~f:(fun h1 h2 -> {
                h1 with
                ivars = Map.merge h1.ivars h2.ivars ~f:(fun ~key r ->
                    Option.some @@ match r with
                    | `Left _ | `Right _ -> assert false
                    | `Both (Set xs, Set ys) -> Set (Set.union xs ys)
                    | `Both (_,Set xs)
                    | `Both  (Set xs,_) -> Set xs
                    | `Both (Top,Top) -> Top)
              }))

let proved hyp pat term = {
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
  open Option.Monad_infix

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

  let sat_arg sat args v : equations =
    Seq.to_list_rev args |>
    List.concat_map ~f:(fun arg ->
        let vars =
          Set.add (Arg.rhs arg |> Exp.free_vars) (Arg.lhs arg) in
        [sat_use v vars])

  let call prog =
    let with_args call f : equations option =
      callee prog call >>= fun sub ->
      let args = Term.enum arg_t sub in
      if Seq.is_empty args then None
      else Some (f args) in

    object
      inherit matcher
      method jmp t r : equations option =
        let match_call call uses =
          with_args call (fun args ->
              List.concat_map uses ~f:(sat_arg sat args)) in
        let match_move call v1 v2 =
          with_args call (fun args -> sat_arg sat args v2) in
        let match_wild call v =
          with_args call (fun args -> sat_arg sat args v) in
        match r, Jmp.kind t with
        | Pat.Call (id,None,uses), Call c
          when our_target id (Call.target c) ->
          match_call c uses
        | Pat.Move (v1,v2), Call c -> match_move c v1 v2
        | Pat.Wild v, Call c -> match_wild c v
        | _ -> None  (* TODO: add Load and Store pats *)

      method def term pat : equations option =
        let match_call id v =
          match Term.get_attr term call_result with
          | Some call when our_target id (Call.target call) ->
            Some [sat_def v (Def.lhs term)]
          | _ -> None in
        match pat with    (* TODO support more than 1 def *)
        | Pat.Call (id,Some v,uses) -> match_call id v
        | _ -> None

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
  let ivars,cvars =
    List.fold constrs ~init:(V.Map.empty,V.Set.empty)
      ~f:(fun (ivars,cvars) cs -> match cs with
          | Constr.Dep (v1,v2) ->
            Map.add ivars ~key:v2 ~data:Top,
            Set.add (Set.add cvars v1) v2
          | Constr.Var (v,_)
          | Constr.Fun (v,_) -> (ivars,Set.add cvars v)) in
  {
    defn; rule = r;
    prems = Pat.Set.of_list (Rule.premises r);
    concs = Pat.Set.of_list (Rule.conclusions r);
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

let pp_hyp ppf h =
  let proofs = Pat.Set.of_list (Map.keys h.proofs) in
  let proved s = Set.is_empty (Set.diff s proofs) in
  let result =
    if proved h.prems && proved h.concs then "was proved" else
    if proved h.prems && not (proved h.concs) then "wasn't proved"
    else "wasn't recognized" in
  Format.fprintf ppf "hypothesis %s %s@."
    (Rule.name h.rule) result


let pp_solution ppf {hyps} =
  List.iter hyps ~f:(fun hyp -> pp_hyp ppf hyp)
