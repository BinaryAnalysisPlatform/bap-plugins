open Core_kernel.Std
open Bap.Std


type taint = tid
type t = taint

module Taints = Tid.Set
module Values = Bil.Result.Id.Map
module SM = Monad.State

open SM.Monad_infix

type taints = Taints.t

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
    super#eval_exp v >>= fun rv ->
    super#eval_store ~mem ~addr v e s >>= fun rr ->
    self#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return rr
    | Bil.Imm a ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_mem a s (ctxt#val_taints rv)) >>= fun () ->
      SM.return rr

  method! load s addr =
    super#load s addr >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint_val r (ctxt#mem_taints addr)) >>= fun () ->
    SM.return r

  method private eval2 e1 e2 r3 =
    super#eval_exp e1 >>= fun r1 ->
    super#eval_exp e2 >>= fun r2 ->
    self#propagate r1 r3 >>= fun () ->
    self#propagate r2 r3 >>= fun () ->
    SM.return r3

  method private eval e rr =
    super#eval_exp e >>= fun re ->
    self#propagate re rr >>= fun () ->
    SM.return rr

  method private propagate rd rr =
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint_val rr (ctxt#val_taints rd))
end

class when_jmp_context p = object
  inherit context
  inherit Biri.context p
end

class ['a] when_jmp_sanitizer = object(self)
  constraint 'a = #when_jmp_context
  inherit ['a] biri as super
  method! eval_jmp jmp =
    super#eval_exp (Jmp.cond jmp) >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#sanitize r) >>= fun () ->
    super#eval_jmp jmp
end

include (Tid : Regular with type t := t)
