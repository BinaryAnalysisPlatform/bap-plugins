open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

module Taint = Tid
module Taints = Taint.Set
module Values = Bil.Result.Id.Map

type taint = Taint.t with bin_io, compare, sexp
type t = taint
type taints = Taint.Set.t with bin_io, compare, sexp
type 'a values = 'a Values.t

let seed = Value.Tag.register
    ~name:"taint_seed"
    ~uuid:"1ab9a363-db8f-4ab4-9fb4-5ff54de97c5c"
    (module Tid)

module Tainted_vars = struct
  type t = taints Var.Map.t with bin_io, compare, sexp
  include Regular.Make(struct
      open Format

      type nonrec t = t with bin_io, compare, sexp
      let module_name = None

      let pp_list pp ppf xs =
        let rec pp_rest ppf = function
          | [] -> ()
          | [x] -> pp ppf x
          | x :: xs -> fprintf ppf "%a,@;%a" pp x pp_rest xs in
        pp_rest ppf xs

      let pp_taints = pp_list Taint.pp

      let pp_taint_set ppf t =
        fprintf ppf "@[<1>[%a@]]" pp_taints (Set.to_list t)

      let pp_binding ppf (v,ts) =
        fprintf ppf "%a => %a" Var.pp v pp_taint_set ts

      let pp_vars ppf t =
        pp_list pp_binding ppf (Map.to_alist t)

      let pp ppf t =
        fprintf ppf "@[<1>{%a@]}" pp_vars t
      let hash = Hashtbl.hash
    end)
end

let vars : taints Var.Map.t tag = Value.Tag.register
    ~name:"taint_vars"
    ~uuid:"03c90a60-e19f-43cc-8049-fdeb23973396"
    (module Tainted_vars)

let create = ident

let get_taints from key = match Map.find from key with
  | None -> Taints.empty
  | Some ts -> ts

let collect_taints =
  Map.fold ~init:Taints.empty ~f:(fun ~key:_ ~data:ts ts' ->
      Set.union ts ts')

class context = object(self)
  val tvs : taints values = Values.empty
  val tas : taints Addr.Map.t = Addr.Map.empty

  (** T(r) <- T(r) U T *)
  method taint_val r ts =
    let tvs' = Values.change tvs (Bil.Result.id r) @@ function
      | None -> Some ts
      | Some ts' -> Some (Taints.union ts ts') in
    {< tvs = tvs' >}

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

  method taints =
    Set.union (collect_taints tvs) (collect_taints tas)
end

let pp_taints ppf taints =
  Taint.Set.iter taints ~f:(Format.fprintf ppf "%a@." Taint.pp)

class ['a] propagator = object(self)
  constraint 'a = #context
  inherit ['a] expi as super

  method! eval_binop op e1 e2 =
    super#eval_binop op e1 e2 >>= self#eval2 e1 e2
  method! eval_unop op e =
    super#eval_unop op e >>= self#eval e
  method! eval_cast ct n e =
    super#eval_cast ct n e >>= self#eval e
  method! eval_concat e1 e2 =
    super#eval_concat e1 e2 >>= self#eval2 e1 e2
  method! eval_extract n1 n2 e =
    super#eval_extract n1 n2 e >>= self#eval e

  method! eval_store ~mem ~addr v e s =
    self#eval_exp v >>= fun rv ->
    super#eval_store ~mem ~addr v e s >>= fun rr ->
    self#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return rr
    | Bil.Imm a ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_mem a s (ctxt#val_taints rv)) >>= fun () ->
      SM.return rr

  method! eval_load ~mem ~addr e s =
    super#eval_load ~mem ~addr e s >>= fun r ->
    super#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return r
    | Bil.Imm a ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_val r (ctxt#mem_taints a)) >>= fun () ->
      SM.return r

  method private eval2 e1 e2 r3 =
    self#eval_exp e1 >>= fun r1 ->
    self#eval_exp e2 >>= fun r2 ->
    self#propagate r1 r3 >>= fun () ->
    self#propagate r2 r3 >>= fun () ->
    SM.return r3

  method private eval e rr =
    self#eval_exp e >>= fun re ->
    self#propagate re rr >>= fun () ->
    SM.return rr

  method private propagate rd rr =
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint_val rr (ctxt#val_taints rd))
end
