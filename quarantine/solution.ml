open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Option.Monad_infix

type hypothesis = tid Pat.Map.t
type hypotheses = hypothesis list
type input = (defn * hypothesis) seq

type proved = Nil with bin_io, compare, sexp
type unproved = Cons of pat * pat list with bin_io, compare, sexp
type undecided = pat list


(** a particular valuation of a given rule  *)
type 'a model = {
  rule : string;
  prem : (pat * tid) list;        (** all premises must hold  *)
  conc : (pat * tid) list;        (** proved conclusions      *)
  miss : 'a;                       (** unproved patterns       *)
} with bin_io, compare, sexp

(** Solution for a given definition. It contains a set of examples
    and a set of counterexamples.

    Based on this two sets we can conclude, that the definition is
    proved to be true, if then set of examples is not empty, while set
    of counter examples is empty. We say, that definition is proved to
    be false, if the set of counterexamples is not empty.  Finally, if
    both sets are empty, then that means that we didn't find a
    matching model in the given program.*)
type solution = {
  examples : proved model list;   (** all formulae are proved       *)
  counters : unproved model list; (** some conclusions are missing  *)
} with bin_io, compare, sexp

type solutions = solution String.Map.t
with bin_io, compare, sexp

type t = solutions with bin_io, compare, sexp

let line = "--------------------------------"

let pp_unpat ppf pat =
  fprintf ppf "%s: %a" "unproved" Pat.pp pat

let pp_unpats ppf pats = List.iter pats ~f:(pp_unpat ppf)

let pp_pat ppf (pat,t) =
  fprintf ppf "%a: %a@;" Tid.pp t Pat.pp pat

let pp_pats ppf pats = List.iter pats ~f:(pp_pat ppf)

let pp_model pp_miss defn ppf m =
  fprintf ppf "@[<v2>rule %s_%s ::=@ %a%s@;%a%a@]@;@;"
    defn m.rule
    pp_pats m.prem line pp_pats m.conc pp_miss m.miss

let pp_nil ppf Nil = ()
let pp_cons ppf (Cons (x,xs)) =
  fprintf ppf "%a%a" pp_unpat x pp_unpats xs

let pp_proved d = pp_model pp_nil d
let pp_unproved d = pp_model pp_cons d
let pp_models pp ppf ms = List.iter ms ~f:(pp ppf)
let pp_examples d ppf s = pp_models (pp_proved d) ppf s.examples
let pp_counters d ppf s = pp_models (pp_unproved d) ppf s.counters
let pp_hypothesis ppf hyp =
  Map.iter hyp ~f:(fun ~key ~data ->
      fprintf ppf "@;%a:%a" Tid.pp data Pat.pp key)

let subset_of x y =
  let set m = Map.data m |> Tid.Set.of_list in
  Set.subset (set x) (set y)

let is x p y = p x y
let isn't x p y = not (p x y)

(** since a term can proof more than one proposition, (i.e., it can
    participate in more than one valuations) we optimistically produce
    more hypotheses than needed. At this step we remove all hypotheses
    that are proper subsets of some other hypotheses (of the same
    definition), since they don't add any more information.*)
let remove_subsets hyps : hypotheses =
  List.filteri hyps ~f:(fun i h1 ->
      is_none (List.findi hyps ~f:(fun j h2 ->
          i <> j && is h1 subset_of h2)))


let model_of_hypothesis rule hyp : undecided model option =
  let prove pat = Map.find hyp pat >>| fun t -> (pat,t) in
  List.map (Rule.premises rule) ~f:prove |> Option.all >>= fun prem ->
  let conc,miss =
    List.partition_map (Rule.conclusions rule) ~f:(fun pat ->
        match prove pat with
        | Some proof -> `Fst proof
        | None -> `Snd pat) in
  Some {rule = Rule.name rule; conc; prem; miss}

let decide_model (m : undecided model) = match m with
  | {miss = []} -> `Fst {m with miss=Nil}
  | {miss = x :: xs} -> `Snd {m with miss = Cons (x,xs)}

let make_solution (hyps : hypotheses) rule =
  let examples,counters =
    remove_subsets hyps |>
    List.filter_map ~f:(model_of_hypothesis rule) |>
    List.partition_map ~f:decide_model in
  {examples;counters}


let join_by_defn : input -> hypotheses Defn.Map.t =
  Seq.fold ~init:Defn.Map.empty ~f:(fun defns (defn,hyp) ->
      Map.change defns defn (function
          | None -> Some [hyp]
          | Some hyps -> Some (hyp :: hyps)))

let index_spec spec =
  List.map spec ~f:(fun d -> Defn.name d, Defn.rules d) |>
  String.Map.of_alist_exn

let merge_solutions s1 s2 = {
  examples = s1.examples @ s2.examples;
  counters = s1.counters @ s2.counters;
}

let create (spec : spec) (input : input) : t =
  let index = index_spec (Spec.defns spec) in
  join_by_defn input |>
  Map.to_sequence |> Seq.filter_map ~f:(fun (defn,hyps) ->
      Map.find index (Defn.name defn) >>|
      List.map ~f:(make_solution hyps) >>=
      List.reduce ~f:merge_solutions >>| fun solution ->
      Defn.name defn,solution) |>
  Seq.to_list_rev |>
  String.Map.of_alist_exn

let is_sat = function
  | {counters=[]; examples=[]} -> None
  | {counters=[]} -> Some true
  | _ -> Some false

let solution t defn = Map.find t (Defn.name defn)

let sat t defn = solution t defn >>= is_sat

let pp pp d ppf t =
  match solution t d with
  | None -> ()
  | Some s -> pp (Defn.name d) ppf s

let pp_sat   d = pp pp_examples d
let pp_unsat d = pp pp_counters d
