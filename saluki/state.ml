open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

open Option.Monad_infix

module Dep = Comparable.Make(struct
    type t = v * v [@@deriving bin_io, compare, sexp]
  end)

(** a complete lattice for dependency equality class.

    Note: the buttom is [Set empty]
*)
type eq =
  | Top              (* superset *)
  | Set of Tid.Set.t

(** Hypothesis.
    If a term matches some pattern in some definition we start to check a
    hypothesis, that this term is a part of something bigger. *)
type hyp = {
  defn    : Defn.t;          (** definition from which we born   *)
  patts   : Pat.Set.t;       (** patterns to be matched          *)
  proofs  : tid Pat.Map.t;   (** map from a patter to matched t  *)
  deps    : eq Dep.Map.t;    (** equivalence classes for inputs  *)
  constrs : constr list;     (** constraints that we mus satisfy *)
  pending : Pat.Set.t;
} [@@deriving fields]


(** State of a proof system. *)
type state = {
  init : hyp list;  (** empty hypothesis, ready to start *)
  hyps : hyp list;  (** active hypothesis, that we're checking  *)
  ts   : Tainter.t;
}

type t = state

(** final state of the  prover  *)
type solution = Solution.t

type matcher = Match.matcher

let hyp_of_defn defn : hyp =
  let constrs = Defn.constrs defn in
  let proofs = Pat.Map.empty in
  let deps =
    List.fold constrs ~init:Dep.Map.empty ~f:(fun deps -> function
        | Constr.Dep (v',v) -> Map.add deps ~key:(v',v) ~data:Top
        | _ -> deps) in
  let (++) pats r = Set.union pats (Pat.Set.of_list r) in
  let patts,pending =
    List.fold (Defn.rules defn)
      ~init:(Pat.Set.empty,Pat.Set.empty) ~f:(fun (pats,pending) r ->
          pats ++ Rule.premises r,
          pending ++ Rule.conclusions r) in
  {defn; patts; deps; proofs; constrs; pending}

let create s t = {
  init = Spec.defns s |> List.map ~f:hyp_of_defn;
  hyps = [];
  ts = t;
}

let start_conclusions s = {
  init = [];
  hyps = List.map s.hyps ~f:(fun hyp -> {
        hyp with patts = Set.union hyp.patts hyp.pending
      });
  ts = s.ts;
}

let taint_of_sort = function
  | S.Reg -> Tainter.regs_of_var
  | S.Ptr -> Tainter.ptrs_of_var

let seed_of_sort = function
  | S.Reg -> Tainter.reg_seed_of_var
  | S.Ptr -> Tainter.ptr_seed_of_var


let debug id term fmt =
  if Tid.(from_string_exn id = Term.tid term)
  then eprintf fmt
  else ifprintf err_formatter fmt

(* BUG: there should be union on taints for OR uses *)

let sat ts term hypo kind v bil : hyp option =
  let dep_use hyp y x =
    List.Assoc.find (Defn.vars hyp.defn) v >>|
    taint_of_sort >>= fun taints ->
    match taints ts (Term.tid term) y with
    | ss -> match Map.find_exn hyp.deps (v,x) with
      | Top -> Some {
          hyp with
          deps = Map.add hyp.deps ~key:(v,x) ~data:(Set ss)
        }
      | Set xs ->
        let ss = Set.inter ss xs in
        if Set.is_empty ss then None
        else Some {
            hyp with
            deps = Map.add hyp.deps ~key:(v,x) ~data:(Set ss)
          } in
  let dep_def hyp bil y =
    List.Assoc.find (Defn.vars hyp.defn) v >>| seed_of_sort >>=
    fun get_seed ->
    match get_seed ts (Term.tid term) bil, Map.find_exn hyp.deps (y,v) with
    | None,Top -> Some {
        hyp with
        deps = Map.add hyp.deps ~key:(y,v)
            ~data:(Set Tid.Set.empty)
      }
    | None,_ -> None
    | Some seed,Top -> Some {
        hyp with
        deps = Map.add hyp.deps ~key:(y,v)
            ~data:(Set (Tid.Set.singleton seed))
      }
    | Some seed, Set seeds ->
      if Set.mem seeds seed then (Some hyp) else None in
  let open Constr in
  List.fold hypo.constrs ~init:(Some hypo) ~f:(fun hyp cs ->
      hyp >>= fun hyp ->
      let sat c = Option.some_if c hyp in
      match cs with
      | Fun (id,v') -> V.(v = v') ==> Predicate.test id term (Bil.var bil) |> sat
      | Var (v',ex) -> V.(v' = v) ==> Var.(ex = bil) |> sat
      | Dep (v1,v2) -> match kind with
        | `def when V.(v2 = v) -> dep_def hyp bil v1
        | `use when V.(v1 = v) -> dep_use hyp bil v2
        | _ -> sat true)

let merge_hyps xs =
  List.reduce_exn xs ~f:(fun h1 h2 -> {
        h1 with
        deps = Map.merge h1.deps h2.deps ~f:(fun ~key r ->
            Option.some @@ match r with
            | `Left _ | `Right _ -> assert false
            | `Both (Set xs, Set ys) -> Set (Set.union xs ys)
            | `Both (_,Set xs) | `Both  (Set xs,_) -> Set xs
            | `Both (Top,Top) -> Top)
      })

let solution ts term hyp (eqs : Match.t) : hyp option =
  let kind v =
    if Set.mem (Defn.ivars hyp.defn) v then `def else `use in
  let rec solve hyp = function
    | Match.All [] -> Some hyp
    | Match.Any [] -> None
    | Match.Eql (0,_) -> Some hyp
    | Match.Eql (v,bil) -> sat ts term hyp (kind v) v bil
    | Match.All constrs -> forall hyp constrs
    | Match.Any constrs -> exists hyp constrs
  and forall hyp constrs =
    List.map constrs ~f:(solve hyp) |> Option.all |> function
    | None -> None
    | Some hyps -> Some (merge_hyps hyps)
  and exists hyp constrs =
    List.filter_map constrs ~f:(solve hyp) |> function
    | [] -> None
    | hyps -> Some (merge_hyps hyps) in
  solve hyp eqs

let decide_hyp ts term hyp matches =
  Set.fold hyp.patts ~init:hyp ~f:(fun hyp pat ->
      if Map.mem hyp.proofs pat then hyp
      else match solution ts term hyp (matches pat) with
        | None -> hyp
        | Some hyp -> {
            hyp with
            proofs = Map.add hyp.proofs ~key:pat ~data:(Term.tid term);
          })

let is_done h = Map.length h.proofs = Set.length h.patts

let check ts t matches h =
  if is_done h then (Some h)
  else
    let h = decide_hyp ts t h matches in
    if Map.is_empty h.proofs then None
    else (Some h)

let step s t matches = {
  s with
  hyps = List.filter_map (s.init @ s.hyps) ~f:(check s.ts t matches)
}

let solution s spec =
  Seq.of_list s.hyps |> Seq.map ~f:(fun {defn;proofs} -> defn,proofs) |>
  Solution.create spec

let pp_proof ppf = function
  | None -> fprintf ppf "unproved"
  | Some p -> fprintf ppf "%a" Tid.pp p

let pp_hyp ppf hyp =
  fprintf ppf "@;@[<v2>hypothesis %s {" (Defn.name hyp.defn);
  Set.iter hyp.patts ~f:(fun p ->
      let proof = Map.find hyp.proofs p in
      fprintf ppf "@;%a: %a" pp_proof proof Pat.pp p);
  fprintf ppf "@]@;}"

let pp_hyps ppf = List.iter ~f:(pp_hyp ppf)

let pp ppf = function
  | {hyps=[]} -> fprintf ppf "No active hypotheses"
  | {hyps} ->
    fprintf ppf "@[<v2>Hypotheses ::= {%a@]@;}"
      pp_hyps hyps
