open Core_kernel.Std
open Bap.Std

class with_jmp p = object
  inherit context
  inherit Biri.context p
end

class ['a] with_jmp = object(self)
  constraint 'a = #context
  constraint 'a = #Biri.context
  inherit ['a] biri as super
  method! eval_jmp jmp =
    super#eval_exp (Jmp.cond jmp) >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#sanitize r) >>= fun () ->
    super#eval_jmp jmp
end
