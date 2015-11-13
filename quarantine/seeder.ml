open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec
open Format
open Utilities

open Option.Monad_infix


let seeded_arg = Value.Tag.register
    ~name:"seeded_arg"
    ~uuid:"657af506-69c7-4534-98db-c33f160aa063"
    (module Arg)

let input_of_constr = function
  | Constr.Dep (_,v) -> Some v
  | _ -> None

let inputs_of_defn rule =
  Defn.constrs rule |>
  List.filter_map  ~f:input_of_constr |>
  V.Set.of_list

let seed_with s t = Term.set_attr t Taint.reg s
let self_seed t = seed_with (Term.tid t) t

let arg_matches intent v cs arg =
  let free = (Arg.rhs arg |> Exp.free_vars) in
  let free = Set.add free (Arg.lhs arg) in
  List.exists cs ~f:(function
      | Constr.Var (v',var) ->
        intent_matches arg intent &&
        V.(v = v') &&
        Set.mem free var
      | _ -> false)

let def_of_arg intent v cs arg =
  require (intent_matches arg intent) >>= fun () ->
  let x = Bil.var (Arg.lhs arg) in
  match Arg.rhs arg with
  | Bil.Var var -> Some (Def.create var x)
  | Bil.Load (Bil.Var m as mem,a,e,s) ->
    Some (Def.create m (Bil.store ~mem ~addr:a x e s))
  | _ -> None

let defs_of_args intent v cs args =
  Seq.filter_map args ~f:(def_of_arg intent v cs)

let tag_arg_def jmp call term =
  seed_with (Term.tid jmp) term

let already_seeded blk var =
  Term.enum def_t blk |> Seq.exists ~f:(fun def ->
      Set.mem (Def.free_vars def) var &&
      Term.has_attr def Taint.reg)

let prepend_def blk def =
  if already_seeded blk (Def.lhs def)
  then blk
  else Term.prepend def_t blk def


let sat_pred constr term v vars =
  Set.exists vars ~f:(fun var ->
      List.for_all constr ~f:(function
          | Constr.Fun (id,v') ->
            V.(v = v') ==> Predicate.test id term var
          | _ -> true))

let seed_jmp prog jmp cons vars sub pat =
  let seed_call intent f id e =
    call_of_jmp jmp     >>= fun caller ->
    require (call_matches caller id) >>= fun () ->
    callee caller prog >>= fun callee ->
    return caller sub  >>| fun return ->
    Term.enum arg_t callee |>
    defs_of_args intent e cons |>
    Seq.map ~f:(tag_arg_def jmp caller) |>
    Seq.fold ~init:return ~f |>
    Term.update blk_t sub in
  let seed_call = seed_call Out prepend_def in
  match pat with
  | Pat.Call (id,0,_) -> sub
  | Pat.Call (id,e,_) when Set.mem vars e ->
    Option.value ~default:sub (seed_call id e)
  | _ -> sub

let seed_def def cons vars blk pat =
  let hit v = Set.mem vars v  in
  let free = Def.free_vars def in
  let pred v = sat_pred cons def v free in
  let def = match pat with
    | Pat.Move (v1,v2)
    | Pat.Load (v1,v2)
    | Pat.Store (v1,v2)
      when pred v1 && pred v2 && (hit v1 || hit v2) ->
      Some (self_seed def)
    | _ -> None in
  match def with
  | None -> blk
  | Some def -> Term.update def_t blk def

let fold_patts spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      let vars = inputs_of_defn defn in
      let cons = Defn.constrs defn in
      Defn.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let rules = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold rules ~init ~f:(fun init rule -> f cons vars init rule)))

let seed_sub (spec : spec) prog sub =
  let sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:sub ~f:(fun sub blk ->
        Term.enum def_t blk |>
        Seq.fold ~init:blk ~f:(fun blk def ->
            fold_patts spec ~init:blk ~f:(seed_def def)) |>
        Term.update blk_t sub) in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts spec ~init:sub ~f:(seed_jmp prog jmp)))

let run spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)
