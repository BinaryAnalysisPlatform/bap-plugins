open Core_kernel.Std
open Bap.Std
open Spec_types

module SM = Monad.State
open SM.Monad_infix

open Format

let def_summary _ = None
let def_mapping _ = None
let def_const = Word.zero 8

let calls_of_spec spec =
  List.filter_map spec ~f:(function
      | `Call call,_ -> Some call
      | _ -> None)

let same_ident ctxt tid = function
  | `Name s -> Tid.name tid = "@"^s
  | `Term t -> Tid.(tid = t)
  | `Addr a -> failwith "Host as addr is not implemented"

let skip () = SM.return false
let pass () = SM.return true

let def_summary call = match Call.target call with
  | Indirect _ -> None
  | Direct tid -> match Tid.name tid with
    | s -> None

class context p k  = object(self)
  val k = k
  val cps : tid list = []
  inherit Taint.context
  inherit Biri.context p as super
  method step =
    if k > 0
    then Some {< k = k - 1 >}
    else None

  method set_restore tid = {< cps = tid :: cps >}
  method pop_restore = match cps with
    | [] -> None
    | c :: cps -> Some (c, {< cps = cps >})

end

let pp_bindings ppf bs =
  Seq.iter bs ~f:(fun (x,v) ->
      fprintf ppf "%a = %a;@;" Var.pp x Bil.Result.pp v)

let print_taints ts =
  eprintf "@[taints = {@;";
  Set.iter ts ~f:(eprintf "%a@;" Taint.pp);
  eprintf "}@;@]"

class ['a] main summary tid_of_addr const hosts spec = object(self)
  constraint 'a = #context
  inherit ['a] biri as super

  method! eval_unknown _ t = self#emit t

  method! lookup v =
    super#lookup v >>= fun r ->
    SM.get () >>= fun ctxt ->
    match Bil.Result.value r with
    | Bil.Imm _ | Bil.Mem _ -> SM.return r
    | Bil.Bot -> self#emit (Var.typ v)

  method! load s a =
    self#concretize_load s a

  method! eval_load ~mem ~addr e s =
    super#eval_load ~mem ~addr e s >>= fun r ->
    self#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return r
    | Bil.Imm a ->
      self#propagate_load a r

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

  method! eval_jmp jmp =
    SM.get () >>= fun ctxt ->
    match ctxt#step with
    | None -> SM.return ()
    | Some ctxt ->
      SM.put ctxt >>= fun () ->
      self#sanitize_jmp jmp >>= fun () ->
      super#eval_jmp jmp

  method eval_call call =
    self#shortcut_indirect call >>= fun () ->
    self#summarize_call call >>= fun () ->
    self#taint_call call

  method! eval_indirect exp =
    self#eval_exp exp >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return ()
    | Bil.Imm dst -> match tid_of_addr dst with
      | Some dst -> self#eval_direct dst
      | None ->
        SM.get () >>= fun ctxt -> match ctxt#pop_restore with
        | None -> super#eval_indirect exp
        | Some (next,ctxt) ->
          SM.put ctxt >>= fun () -> self#eval_direct next

  method private sanitize_jmp jmp =
    self#eval_exp (Jmp.cond jmp) >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#sanitize r)

  method private eval_host (name,args) call  =
    SM.get () >>= fun ctxt -> match Call.target call with
    | Indirect _ -> SM.return ()
    | Direct tid when not(same_ident ctxt tid name) -> SM.return ()
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

  method private set_arg x v =
    SM.get () >>= fun ctxt ->
    let ctxt, r = ctxt#create_word v in
    self#update x r >>= fun () ->
    SM.put ctxt

  method private infect host x =
    SM.get () >>= fun ctxt -> match ctxt#trace with
    | [] -> SM.return ()
    | term :: _ ->
      let taint = Taint.create (`Call host) term in
      SM.put (ctxt#taint_val x (Taint.Set.singleton taint))

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

  method private concretize_load s a =
    super#load s a >>= fun r -> match Bil.Result.value r with
    | Bil.Imm _ | Bil.Mem _ -> SM.return r
    | Bil.Bot -> self#emit_const 8

  method private propagate_load addr r =
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#taint_val r (ctxt#mem_taints addr)) >>= fun () ->
    SM.return r

  method private taint_call call =
    List.fold hosts ~init:(SM.return ()) ~f:(fun m host ->
        m >>= fun () -> self#eval_host host call) >>= fun () ->
    SM.return ()

  method private shortcut_indirect call =
    match Call.target call with
    | Direct _ -> self#call_with_restore call
    | Indirect _ -> match Call.return call with
      | None -> super#eval_call call
      | Some lab -> super#eval_ret lab

  method private call_with_restore call =
    match Call.return call with
    | None | Some (Indirect _) -> super#eval_call call
    | Some (Direct ret) ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#set_restore ret) >>= fun () ->
      super#eval_call call

  method private summarize_call call =
    let create f =
      SM.get () >>= fun ctxt ->
      let ctxt, v = f ctxt in
      SM.put ctxt >>= fun () ->
      SM.return v in
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

exception Starting_point_not_found

let run_from_tid p (biri : 'a #main) tid =
  match Program.lookup sub_t p tid with
  | Some sub -> biri#eval_sub sub
  | None -> raise Starting_point_not_found

let tid_of_name str =
  match Tid.from_string ("@"^str) with
  | Ok tid -> tid
  | Error _ -> raise Starting_point_not_found

let tid_of_ident mapping = function
  | `Term tid -> tid
  | `Name str -> tid_of_name str
  | `Addr add -> match mapping add with
    | None -> raise Starting_point_not_found
    | Some tid -> tid

let run_from_point mapping p biri point =
  run_from_tid p biri (tid_of_ident mapping point)

let run p k spec point =
  let ctxt = new context p k in
  let biri = new main def_summary def_mapping def_const (calls_of_spec spec) spec in
  let map _ = None in
  let res = run_from_point map p biri point in
  SM.exec res ctxt
