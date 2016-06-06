open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Option.Monad_infix

type hypothesis = tid Pat.Map.t [@@deriving bin_io, compare, sexp]
type hypotheses = hypothesis list
type input = (defn * hypothesis) seq
type proof = (pat * tid)  [@@deriving bin_io, compare, sexp]
type proofs = proof list * pat list
  [@@deriving bin_io, compare, sexp]



(** a particular valuation of a given rule  *)
type model = {
  rule : string;
  prem : proofs;        (** all premises must hold  *)
  conc : proofs;        (** proved conclusions      *)
} [@@deriving bin_io, compare, sexp]

(** Solution for a given definition. It contains a set of examples
    and a set of counterexamples.

    Based on this two sets we can conclude, that the definition is
    proved to be true, if then set of examples is not empty, while set
    of counter examples is empty. We say, that definition is proved to
    be false, if the set of counterexamples is not empty.  Finally, if
    both sets are empty, then that means that we didn't find a
    matching model in the given program.*)
type solution = {
  examples : model list; (** all formulae are proved       *)
  counters : model list; (** some conclusions are missing  *)
} [@@deriving bin_io, compare, sexp]

type solutions = solution String.Map.t
  [@@deriving bin_io, compare, sexp]

type t = solutions [@@deriving bin_io, compare, sexp]

let line = "--------------------------------"

let pp_unpat ppf pat =
  fprintf ppf "%s: %a@;" "unproved" Pat.pp pat

let pp_unpats ppf pats = List.iter pats ~f:(pp_unpat ppf)

let pp_pat ppf (pat,t) =
  fprintf ppf "%a: %a@;" Tid.pp t Pat.pp pat

let pp_pats ppf (p,u) =
  List.iter p ~f:(pp_pat ppf);
  List.iter u ~f:(pp_unpat ppf)

let pp_model defn ppf m =
  fprintf ppf "@[<v2>rule %s_%s ::=@ %a%s@;%a@]@;@;"
    defn m.rule
    pp_pats m.prem line pp_pats m.conc


let pp_proved d = pp_model d
let pp_unproved d = pp_model d
let pp_models pp ppf ms = List.iter ms ~f:(pp ppf)
let pp_examples d ppf s = pp_models (pp_proved d) ppf s.examples
let pp_counters d ppf s = pp_models (pp_unproved d) ppf s.counters
let pp_hypothesis ppf hyp =
  Map.iteri hyp ~f:(fun ~key ~data ->
      fprintf ppf "@;%a:%a" Tid.pp data Pat.pp key)

let dedup hyps : hypotheses =
  List.dedup ~compare:(Pat.Map.compare Tid.compare) hyps

let prove f rule hyp =
  let prove pat = Map.find hyp pat >>| fun t -> (pat,t) in
  List.partition_map (f rule) ~f:(fun pat ->
      match prove pat with
      | Some proof -> `Fst proof
      | None -> `Snd pat)

let model_of_hypothesis rule hyp : model =
  let prem = prove Rule.premises rule hyp in
  let conc = prove Rule.conclusions rule hyp in
  {rule = Rule.name rule; conc; prem}

let decide_model (m : model) = match m with
  | {prem = (_,_ :: _)} -> `Fst m
  | {conc = (_,[])} -> `Fst m
  | _ -> `Snd m

let make_solution (hyps : hypotheses) rule =
  let examples,counters =
    List.map (dedup hyps) ~f:(model_of_hypothesis rule) |>
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

let annotate_term defn anns (pat,tid) =
  let data = match String.split defn ~on:'/' with
    | x :: _ -> x
    | _ -> defn in
  Map.add_multi anns ~key:tid ~data

let annotate_model defn anns ({prem=(prem,_); conc=(conc,_)}) =
  List.fold ~init:anns (prem @ conc) ~f:(annotate_term defn)

let annotate_models defn anns models=
  List.fold ~init:anns models ~f:(annotate_model defn)

let annotate_solution sat defn anns sol = match sat with
  | `sat -> annotate_models defn anns sol.examples
  | `uns -> annotate_models defn anns sol.counters


let term_sat = Value.Tag.register (module String)
    ~name:"saluki-sat"
    ~uuid:"122f4fb8-80f3-4e92-b254-def6e52d4f40"

let term_uns = Value.Tag.register (module String)
    ~name:"saluki"
    ~uuid:"dfdd406d-8c19-4c4f-9227-b4e625282f7a"

let tag = function
  | `sat -> term_sat
  | `uns -> term_uns

let annotate_with  sns (t : t) prog =
  let anns = Map.fold t ~init:Tid.Map.empty
      ~f:(fun ~key:defn ~data:sol anns ->
          annotate_solution sns defn anns sol) in
  let mapper = object
    inherit Term.mapper as super
    method! map_term cls t =
      let t = super#map_term cls t in
      match Map.find anns (Term.tid t) with
      | None -> t
      | Some comms ->
        Term.set_attr t (tag sns) @@
        String.concat ~sep:" " comms

  end in
  mapper#run prog

let annotate t prog =
  annotate_with `sat t prog |>
  annotate_with `uns t
