open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

let def_const = Word.zero 8


class ['a] main ?(memory=fun _ -> None) ?(const=def_const) () =
  object(self)
    inherit ['a] expi as super
    method! eval_unknown _ t = self#emit t

    method! lookup v =
      super#lookup v >>= fun r ->
      match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> self#emit (Var.typ v)

    method! load s a =
      super#load s a >>= fun r -> match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> match memory a with
        | None -> self#emit_const 8
        | Some w ->
          SM.get () >>= fun ctxt ->
          let ctxt,r = ctxt#create_word w in
          SM.put ctxt >>= fun () ->
          SM.return r

    method private emit = function
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
