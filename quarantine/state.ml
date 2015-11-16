open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

open Option.Monad_infix

(** a lattice for dependency equality class.*)
type eq =
  | Top              (* superset *)
  | Set of Tid.Set.t (* invariant: set is not empty *)

(** Hypothesis.
    If a term matches some pattern in some definition we start to check a
    hypothesis, that this term is a part of something bigger. *)
type hyp = {
  defn    : Defn.t;          (** definition from which we born   *)
  patts   : Pat.Set.t;       (** patterns to be matched          *)
  proofs  : tid Pat.Map.t;   (** map from a patter to matched t  *)
  ivars   : eq V.Map.t;      (** equivalence classes for inputs  *)
  constrs : constr list;     (** constraints that we mus satisfy *)
} with fields


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
  let ivars =
    Set.fold (Defn.ivars defn) ~init:V.Map.empty ~f:(fun ivars v ->
        Map.add ivars ~key:v ~data:Top) in
  let patts =
    List.fold (Defn.rules defn) ~init:Pat.Set.empty ~f:(fun pats r ->
        List.rev_append (Rule.conclusions r) (Rule.premises r) |>
        List.fold ~init:pats ~f:(fun pats pat ->
            Set.add pats pat)) in
  {defn; patts; ivars; proofs; constrs;}

let create s t = {
  init = Spec.defns s |> List.map ~f:hyp_of_defn;
  hyps = [];
  ts = t;
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

(* to answer a question whether a given term belongs to some
   dependency equivalence class we need to know classes for each
   variable that is used or defined by a given term.

   So far, the class of a variable, defined by a term is assumed to be
   equal to the term's TID. This will break as soon as term will
   define more than one term.

   The class of a variable, used by the term, is stored in its
   Taint.map.

   The problem arises when we're trying to handle call terms, that
   define multiple values, as well as use values. Both are inserted
   explicitly as definitions terms before and after the call term. So
   the call itself doesn't contain any Taint.maps or seeds.

   Instead of extracting Taint.maps and seeds from a term, we can
   create an explicit map of type

      [term:tid -> var:tid -> taint set]

   We actually will need three such maps - for register allocated
   data, pointers, and seeds (since the call terms introduce terms,
   that can define more than one value at once).

   If we're going to stick with a map solution, then we need the
   following:

   1. definitions seeded from some call should have some mechanism
      to be identified. So far, they have a seed attribute, that is
      equal to the term id of the call, that they were seeded from.
      Also they have argument on their left-hand side if they're uses,
      and argument on right hand side, if they're definitions. Looks
      like that it is enough.

*)
let sat ts term hyp kind v bil : hyp option =
  let dep_use y x =
    List.Assoc.find (Defn.vars hyp.defn) v >>|
    taint_of_sort >>= fun taints ->
    match taints ts (Term.tid term) y with
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
  let dep_def bil x =
    List.Assoc.find (Defn.vars hyp.defn) v >>|
    seed_of_sort >>= fun get_seed ->
    get_seed ts (Term.tid term) bil >>= fun seed ->
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
      | Fun (id,v') -> V.(v = v') ==> Predicate.test id term bil |> sat
      | Var (v',ex) -> V.(v' = v) ==> Var.(ex = bil) |> sat
      | Dep (v1,v2) ->  match kind with
        | `def when V.(v2 = v) -> dep_def bil v2
        | `use when V.(v1 = v) -> dep_use bil v2
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

let solution ts term hyp (eqs : Match.t) : hyp option =
  let kind v = if Map.mem hyp.ivars v then `def else `use in
  let rec solve hyp = function
    | Match.All [] -> Some hyp
    | Match.Any [] -> None
    | Match.Eql (0,_) -> Some hyp
    | Match.Eql (v,bil) -> sat ts term hyp (kind v) v bil
    | Match.All constrs -> forall hyp constrs
    | Match.Any constrs -> exists hyp constrs
  and forall hyp constrs =
    List.map constrs ~f:(solve hyp) |>
    Option.all |> function
    | None -> None
    | Some hyps -> Some (merge_hyps hyps)
  and exists hyp constrs =
    List.filter_map constrs ~f:(solve hyp) |> function
    | [] -> None
    | hyps -> Some (merge_hyps hyps) in
  solve hyp eqs

let proved hyp pat term = {
  hyp with
  proofs = Map.add hyp.proofs ~key:pat ~data:((Term.tid term));
}

let decide_hyp ts term hyp matches =
  Set.fold hyp.patts ~init:hyp ~f:(fun hyp pat ->
      if Map.mem hyp.proofs pat then hyp
      else match solution ts term hyp (matches pat) with
        | Some hyp -> proved hyp pat term
        | None -> {hyp with patts = Set.add hyp.patts pat})

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
