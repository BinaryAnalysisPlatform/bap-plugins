open Core_kernel.Std
open Bap.Std


module SM = Monad.State
open SM.Monad_infix

class ['a] constant const = object(self)
  constraint 'a = #Expi.context
  inherit ['a] expi as super

  method! eval_unknown _ t = self#emit t

  method! lookup v =
    super#lookup v >>= fun r -> match Bil.Result.value r with
    | Bil.Imm _ | Bil.Mem _ -> SM.return r
    | Bil.Bot -> self#emit (Var.typ v)

  method private emit t =
    match t with
    | Type.Imm sz -> self#emit_const sz
    | Type.Mem _  -> self#emit_empty

  method private emit_const sz =
    SM.get () >>= fun ctxt ->
    let const = Word.extract_exn ~lo:0 ~hi:(sz-1) const in
    let ctxt,r = ctxt#create_word const in
    SM.put ctxt >>= fun () ->
    SM.return r

  method private emit_empty =
    SM.get () >>= fun ctxt ->
    let ctxt,r = ctxt#create_storage self#empty in
    SM.put ctxt >>= fun () ->
    SM.return r

end
