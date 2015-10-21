open Core_kernel.Std
open Bap.Std


module SM = Monad.State
open SM.Monad_infix

type summary = (var * Bil.value) list


module Tricks = struct
  class ['a] constant const = object(self)
    constraint 'a = #Expi.context
    inherit ['a] expi as super

    method! eval_unknown _ t = self#emit t

    method! lookup v =
      super#lookup v >>= fun r -> match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> self#emit (Var.typ v)

    method! load s a =
      super#load s a >>= fun r -> match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> self#emit_const 8

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


  class ['a] indirects_from_mapping tid_of_addr = object(self)
    constraint 'a = #Biri.context
    inherit ['a] biri as super
    method! eval_indirect exp =
      super#eval_exp exp >>| Bil.Result.value >>= function
      | Bil.Bot | Bil.Mem _ -> SM.return ()
      | Bil.Imm dst -> match tid_of_addr dst with
        | None -> SM.return ()
        | Some dst -> super#eval_direct dst
  end


  class ['a] shortcut_indirect_calls = object(self)
    inherit ['a] biri as super
    method! eval_call call = match Call.target call with
      | Direct _ -> super#eval_call call
      | Indirect _ -> match Call.return call with
        | None -> super#eval_call call
        | Some lab -> super#eval_ret lab
  end

  let create f =
    SM.get () >>= fun ctxt ->
    let ctxt, v = f ctxt in
    SM.put ctxt >>= fun () ->
    SM.return v

  class ['a] summarize_calls summary = object(self)
    inherit ['a] biri as super
    method! eval_call call =
      match summary call with
      | None -> super#eval_call call
      | Some summary ->
        List.fold summary ~init:(SM.return ()) ~f:(fun m (x,v) ->
            m >>= fun () -> create (fun ctxt -> match v with
                | Bil.Mem s -> ctxt#create_storage s
                | Bil.Imm w -> ctxt#create_word w
                | Bil.Bot   -> ctxt#create_undefined) >>= fun r ->
            self#update x r)
  end
end

open Tricks


let def_summary _ = None
let def_mapping _ = None
let def_const = Word.zero 8

class ['a] t
    ?(summary=def_summary)
    ?(mapping=def_mapping)
    ?(const=def_const)
    () =
  object(self)
    inherit ['a] biri
    inherit ['a] shortcut_indirect_calls
    inherit ['a] constant const
    inherit ['a] summarize_calls summary
    inherit ['a] indirects_from_mapping mapping
  end
