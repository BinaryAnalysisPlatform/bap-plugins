open Core_kernel.Std
open Bap.Std
open Spec_types


type taint = {
  term : tid;
  host : host;
} with bin_io, compare, sexp, fields

let create host term = {host; term}

module Taint = Regular.Make(struct
    type t = taint with compare, bin_io, sexp
    let hash t = Tid.hash t.term
    let module_name = Some "Taint"

    open Format

    let rec pp ppf t =
      fprintf ppf "%a@%a" pp_host t.host Tid.pp t.term
    and pp_call ppf (id,_) = match id with
      | `Name n -> fprintf ppf "%s" n
      | `Term t -> Tid.pp ppf t
      | `Addr a -> Addr.pp ppf a
    and pp_read ppf = function
      | `Var v -> pp_var ppf v
      | `Mem e -> pp_mem ppf e
    and pp_seq ppf (h1,h2) =
      fprintf ppf "%a..%a" pp_host h1 pp_host h2
    and pp_host ppf = function
      | `Call c -> pp_call ppf c
      | `Read e -> pp_read ppf e
      | `Seq s  -> pp_seq ppf s
    and pp_mem ppf (v,n) = fprintf ppf "%a[%d]" pp_var v n
    and pp_var ppf = function
      | `Reg v -> Var.pp ppf v
      | `Pos n -> fprintf ppf "$%d" n
      | `Ret   -> fprintf ppf "$?"
      | `All   -> fprintf ppf "$*"
  end)


type t = taint
type taints = Taint.Set.t
module Taints = Taint.Set
module Values = Bil.Result.Id.Map
type 'a values = 'a Values.t


let get_taints from key = match Map.find from key with
  | None -> Taints.empty
  | Some ts -> ts

let collect_taints =
  Map.fold ~init:Taints.empty ~f:(fun ~key:_ ~data:ts ts' ->
      Set.union ts ts')

class context = object(self)
  inherit Expi.context
  val tvs : taints values = Values.empty
  val tas : taints Addr.Map.t = Addr.Map.empty
  val san : taints = Taints.empty
  val tot : taints = Taints.empty

  (** T(r) <- T(r) U T *)
  method taint_val r ts =
    let tvs' = Values.change tvs (Bil.Result.id r) @@ function
      | None -> Some ts
      | Some ts' -> Some (Taints.union ts ts') in
    let tot' = Taints.union tot ts in
    {< tvs = tvs'; tot = tot' >}

  method taint_mem a (s : size) ts =
    let addrs = Seq.init (Size.to_bytes s) ~f:(fun n -> Addr.(a++n)) in
    let tas' = Seq.fold addrs ~init:tas ~f:(fun tas a ->
        Map.change tas a (function
            | None -> Some ts
            | Some ts' -> Some (Set.union ts ts'))) in
    {< tas = tas' >}

  (** T(r) = { t : t |-> v}  *)
  method val_taints r = get_taints tvs (Bil.Result.id r)
  method mem_taints r = get_taints tas r

  (** T := T \ T(v)  *)
  method sanitize r =
    let ts = self#val_taints r in
    let clean = Map.map  ~f:(fun ts' -> Taints.diff ts' ts) in
    {< tvs = clean tvs; tas = clean tas; san = Set.union ts san >}

  method sanitized = san
  method all_taints = tot

  method live_taints =
    Set.union (collect_taints tvs) (collect_taints tas)
end

let pp_taints ppf taints =
  Taint.Set.iter taints ~f:(Format.fprintf ppf "%a@." Taint.pp)


let compute_result (ctxt : #context)  =
  let checked = ctxt#sanitized in
  let all_taints = ctxt#all_taints in
  let maybe = Set.diff all_taints checked in
  let live = ctxt#live_taints in
  let dead = Set.diff maybe live in
  `Cured checked, `Uncured (Set.inter maybe live), `Dead dead


include Taint
