open Core_kernel.Std
open Bap.Std

type taint = tid

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

class tainted_context p = object(self)
  inherit Biri.context p
  val tvs : taints values = Values.empty
  val tas : taints Addr.Map.t = Addr.Map.empty
  val san : taints = Taints.empty
  val tot : taints = Taints.empty

  (** T(r) <- T(r) U T *)
  method private taint r ts =
    let tvs' = Values.change tvs (Bil.Result.id r) @@ function
      | None -> Some ts
      | Some ts' -> Some (Taints.union ts ts') in
    let tot' = Taints.union tot ts in
    {< tvs = tvs'; tot = tot' >}

  (** T(r) = { t : t |-> v}  *)
  method vtaints r = get_taints tvs (Bil.Result.id r)
  method ataints r = get_taints tas r

  method introduce r t =
    self#taint r (Taints.singleton t)

  (** T := T \ T(v)  *)
  method sanitize r =
    let ts = self#vtaints r in
    let clean = Map.map  ~f:(fun ts' -> Taints.diff ts' ts) in
    {< tvs = clean tvs; tas = clean tas; san = Set.union ts san >}

  method to_addr r a (s : size) =
    let ts = self#vtaints r in
    let addrs = Seq.init (Size.to_bytes s) ~f:(fun n -> Addr.(a++n)) in
    let tas' = Seq.fold addrs ~init:tas ~f:(fun tas a ->
        Map.change tas a (function
            | None -> Some ts
            | Some ts' -> Some (Set.union ts ts'))) in
    {< tas = tas' >}

  method sanitized = san
  method all_taints = tot

  method live_taints =
    Set.union (collect_taints tvs) (collect_taints tas)
end

class ['a] taint_propagator = object(self)
  constraint 'a = #tainted_context
  inherit ['a] biri as super

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
    | Bil.Imm w ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_addr rr w s) >>= fun () ->
      SM.return rr

  method! load s addr =
    super#load s addr >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint r (ctxt#ataints addr)) >>= fun () ->
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
    SM.put (ctxt#taint rr (ctxt#vtaints rd))
end

class ['a] sanitizer = object(self)
  constraint 'a = #tainted_context
  inherit ['a] biri as super

  method! eval_jmp jmp =
    super#eval_exp (Jmp.cond jmp) >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#sanitize r) >>= fun () ->
    super#eval_jmp jmp
end


class main_context k p = object
  inherit tainted_context p
  val k = k
  method step = if k > 0 then Some {< k = k - 1 >} else None
end

class ['a] main k p = object
  constraint 'a = #main_context
  inherit ['a] taint_propagator
  inherit ['a] sanitizer
end


let to_result (ctxt : #main_context) =
  let checked = ctxt#sanitized in
  let all_taints = ctxt#all_taints in
  let maybe = Set.diff all_taints checked in
  let live = ctxt#live_taints in
  let dead = Set.diff maybe live in
  checked, Set.inter maybe live, dead
