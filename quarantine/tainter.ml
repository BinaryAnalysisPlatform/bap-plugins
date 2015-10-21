open Core_kernel.Std
open Bap.Std
open Spec_types

module SM = Monad.State
open SM.Monad_infix

let same_ident ctxt tid = function
  | `Name s -> Tid.name tid = s
  | `Term t -> Tid.(tid = t)
  | `Addr a -> failwith "Host as addr is not implemented"

type 'a u = 'a Bil.Result.u

class ['a] call (hosts : (ident * args) list) = object(self)
  constraint 'a = #Taint.context
  constraint 'a = #Biri.context
  inherit ['a] biri as super

  method! eval_call call : 'a u =
    super#eval_call call >>= fun () ->
    List.fold hosts ~init:(SM.return ()) ~f:(fun m host ->
        m >>= fun () -> self#eval_host host call) >>= fun () ->
    SM.return ()

  method private eval_host (name,args) call : 'a u =
    SM.get () >>= fun ctxt -> match Call.target call with
    | Indirect _ -> SM.return ()
    | Direct tid when same_ident ctxt tid name -> SM.return ()
    | Direct tid ->
      let args = if args = [] then [`Var `Ret,None] else args in
      List.fold  args ~init:(SM.return ())
        ~f:(fun m arg -> m >>= fun () -> match arg with
          | `Var (`Reg x), Some v ->
            self#set_arg x v >>= fun () ->
            self#lookup x >>= fun r ->
            self#infect (name,args) r
          | `Var (`Reg x), None ->
            self#lookup x >>= self#infect (name,args)
          | _ -> failwith "Unsupported host spec")

  method private set_arg x v : 'a u =
    SM.get () >>= fun ctxt ->
    let ctxt, r = ctxt#create_word v in
    self#update x r >>= fun () ->
    SM.put ctxt

  method private infect host x : 'a u =
    SM.get () >>= fun ctxt -> match ctxt#trace with
    | [] -> SM.return ()
    | term :: _ ->
      let taint = Taint.create (`Call host) term in
      SM.put (ctxt#taint_val x (Taint.Set.singleton taint))
end
