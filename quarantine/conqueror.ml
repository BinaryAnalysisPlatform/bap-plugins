open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

open Format


let def_summary _ = None
let def_const = Word.zero 8


let skip () = SM.return false
let pass () = SM.return true

let summaries = String.Map.of_alist_exn [
  ]

let def_summary call = match Call.target call with
  | Indirect _ -> None
  | Direct tid ->
    Map.find summaries (Tid.name tid)

let target_of_goto jmp = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | _ -> None

let visit vis tid = Map.change vis tid ~f:(function
    | None   -> Some 1
    | Some n -> Some (n+1))

let create_mapping prog =
  let addrs = Addr.Table.create () in
  let add t a = Hashtbl.set addrs ~key:a ~data:(Term.tid t) in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter  ~f:(fun blk ->
          match Term.get_attr blk Disasm.block with
          | Some addr -> add blk addr
          | None -> ());
      match Term.get_attr sub subroutine_addr with
      | Some addr -> add sub addr
      | None -> ());
  Hashtbl.find addrs


class ['a] concretizer ?(memory=fun _ -> None) ?(const=def_const) () =
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

type 's checkpoint = tid * 's

let merge_visited = Map.merge ~f:(fun ~key -> function
    | `Left x | `Right x -> Some x
    | `Both (x,y) -> Some (Int.max x y))

class context
    ?(max_steps=1000)
    ?(max_loop_length=max_steps / 100) p  = object(self : 's)
  inherit Biri.context p

  val blk : blk term option = None
  val callstack : sub term list = []
  val k = max_steps
  val vis : int Tid.Map.t = Tid.Map.empty (* visited *)
  val cps : 's checkpoint list Tid.Map.t = Tid.Map.empty
  val rets : tid list = []

  method visited = vis
  method checkpoints = cps

  method backtrack = match List.hd callstack with
    | None -> (* warning: broken stack *)None
    | Some sub ->
      let key = Term.tid sub in
      match Map.find cps key with
      | None -> None
      | Some ps -> match List.filter ps ~f:(fun (p,_) -> not(Map.mem vis p)) with
        | [] -> None
        | (p,ctxt) :: ps ->
          let self = {< cps = Map.add cps ~key ~data:ps >} in
          let self = ctxt#merge self in
          Some (self#set_next (Some p))

  method add_checkpoint p = match List.hd callstack with
    | None -> (* warning: broken stack *) self
    | Some sub ->
      let key = Term.tid sub in
      {< cps = Map.add_multi cps ~key ~data:(p,self) >}

  method merge runner = {<
    vis = runner#visited;
    cps = runner#checkpoints;
  >}


  method store_return ret = {< rets = ret :: rets >}

  method return = match rets with
    | [] -> self#set_next None
    | r :: rs -> {< rets = rs >}#set_next (Some r)

  method blk = blk
  method enter_blk blk = {< blk = Some blk >}
  method enter_sub sub = {< callstack = sub :: callstack >}

  method leave_sub (_ : sub term) = match callstack with
    | _ :: top -> {< callstack = top >}
    | [] -> {< callstack = [] >}

  method step = if k > 0
    then Some {< k = k - 1 >}
    else None

  method visit_term tid = {< vis = visit vis tid >}

  method will_loop tid = match callstack with
    | [] -> false
    | sub :: _ -> match Map.find vis tid with
      | None -> false
      | Some n -> n > max_loop_length && Term.find blk_t sub tid <> None

  method will_return tid = match callstack with
    | _ :: par :: _ -> Term.find blk_t par tid <> None
    | _ -> false
end

class ['a] main ?(summary=def_summary) p =
  let tid_of_addr = create_mapping p in
  object(self)
    constraint 'a = #context
    inherit ['a] Biri.t as super

    method! enter_term cls t =
      let tid = Term.tid t in
      SM.get () >>= fun c ->
      SM.put (c#visit_term tid) >>= fun () ->
      super#enter_term cls t

    method! leave_term cls t =
      super#leave_term cls t >>= fun () ->
      SM.get () >>= fun ctxt -> match ctxt#next with
      | Some tid -> SM.return ()
      | None -> SM.return ()

    method! eval_blk blk =
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#enter_blk blk) >>= fun () ->
      super#eval_blk blk

    method! eval_sub sub =
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#enter_sub sub) >>= fun () ->
      super#eval_sub sub >>= fun () ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#leave_sub sub)

    method! eval_jmp jmp =
      SM.get () >>= fun ctxt ->
      match ctxt#step with
      | None -> self#stop
      | Some ctxt ->
        SM.put ctxt >>= fun () ->
        super#eval_jmp jmp >>= fun () ->
        self#add_checkpoints >>= fun () ->
        SM.get () >>= fun ctxt ->
        match ctxt#next with
        | Some dst when ctxt#will_loop dst -> self#return
        | Some dst when ctxt#will_return dst -> self#return
        | None -> self#return
        | _ -> SM.return ()

    method private stop =
      SM.get () >>= fun ctxt -> SM.put (ctxt#set_next None)

    method private return =
      SM.get () >>= fun ctxt -> match ctxt#backtrack with
      | None -> SM.put ctxt#return
      | Some next -> SM.put next

    method private add_checkpoints =
      SM.get () >>= fun ctxt ->
      match ctxt#blk with
      | None -> assert false
      | Some blk ->
        Term.enum jmp_t blk |>
        Seq.fold ~init:(SM.return ()) ~f:(fun m jmp ->
            m >>= fun () ->
            SM.get () >>= fun ctxt ->
            SM.put (ctxt#visit_term (Term.tid jmp)) >>= fun () ->
            self#next_of_jmp jmp >>= function
            | None -> SM.return ()
            | Some tid ->
              SM.get () >>= fun ctxt ->
              SM.put (ctxt#add_checkpoint tid))

    method private next_of_jmp jmp =
      let goto dst =
        self#eval_goto dst >>= fun () ->
        SM.get () >>| fun ctxt -> ctxt#next in
      SM.get () >>= fun ctxt ->
      let next = match Jmp.kind jmp with
        | Int _ -> SM.return None
        | Goto dst | Ret dst -> goto dst
        | Call dst -> goto (Call.target dst) in
      SM.put ctxt >>= fun () ->
      next

    method! eval_call call =
      self#shortcut_indirect call >>= fun () ->
      self#summarize_call call

    method! eval_indirect exp =
      self#eval_exp exp >>| Bil.Result.value >>= function
      | Bil.Bot | Bil.Mem _ -> self#stop
      | Bil.Imm dst -> match tid_of_addr dst with
        | Some dst -> self#eval_direct dst
        | None -> self#stop

    method! eval_exn _ ret = self#eval_direct ret


    method private shortcut_indirect call =
      match Call.target call with
      | Direct _ -> self#call_with_restore call
      | Indirect _ -> match Call.return call with
        | None -> self#stop
        | Some ret -> super#eval_ret ret

    method private call_with_restore call =
      match Call.return call with
      | None | Some (Indirect _) -> super#eval_call call
      | Some (Direct ret) ->
        SM.get () >>= fun ctxt ->
        SM.put (ctxt#store_return ret) >>= fun () ->
        super#eval_call call

    method private summarize_call call =
      let create f =
        SM.get () >>= fun ctxt ->
        let ctxt, v = f ctxt in
        SM.put ctxt >>= fun () ->
        SM.return v in
      match summary call with
      | None -> super#eval_call call
      | Some summary -> match Call.return call with
        | None -> self#stop
        | Some ret ->
          List.fold summary ~init:(SM.return ()) ~f:(fun m (x,v) ->
              m >>= fun () -> create (fun ctxt -> match v with
                  | Bil.Mem s -> ctxt#create_storage s
                  | Bil.Imm w -> ctxt#create_word w
                  | Bil.Bot   -> ctxt#create_undefined) >>= fun r ->
              self#update x r) >>= fun () ->
          self#eval_ret ret
  end
