open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

class ['a] jmp = object(self)
  constraint 'a = #Taint.context
  constraint 'a = #Biri.context
  inherit ['a] biri as super
  method! eval_jmp jmp =
    super#eval_exp (Jmp.cond jmp) >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#sanitize r) >>= fun () ->
    super#eval_jmp jmp
end
