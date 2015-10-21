open Core_kernel.Std
open Bap.Std
open Spec_types

module SM = Monad.State
open SM.Monad_infix

class ['a] t = object(self)
  constraint 'a = #Taint.context
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
