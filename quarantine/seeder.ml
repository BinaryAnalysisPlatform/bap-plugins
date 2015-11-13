open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

open Option.Monad_infix

let seeded_arg = Value.Tag.register
    ~name:"seeded_arg"
    ~uuid:"657af506-69c7-4534-98db-c33f160aa063"
    (module Arg)

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

(* TODO: this should be reimplemented, to follow
   this algorithm:

   1. foreach variable
   1.1 find argument with matching position
   1.2 do not forget to check other constraints
       like predicates and vars
   1.2 create argument
   1.3 ....
   1.4 PROFIT

   see you in Sunday :(
*)
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

let append_def blk def =
  (* TODO: prepend only if not seeded already *)
  Term.append def_t blk def


let sat_pred constr term v vars =
  Set.exists vars ~f:(fun var ->
      List.for_all constr ~f:(function
          | Constr.Fun (id,v') ->
            V.(v = v') ==> Predicate.test id term var
          | _ -> true))

let nullify_if xs ~f =
  List.map xs ~f:(fun x -> if f x then 0 else x)

let nullify_inputs ivars = nullify_if ~f:(Set.mem ivars)
let nullify_outputs ivars =
  nullify_if ~f:(Fn.non (Set.mem ivars))

let seed_jmp prog jmp cons ivars sub pat =
  let seed_call intent inserter target id vs sub =
    call_of_jmp jmp  >>= fun caller ->
    require (call_matches caller id) >>= fun () ->
    callee caller prog >>= fun callee ->
    target caller sub  >>| fun blk ->
    Term.enum arg_t callee |>
    defs_of_args intent vs cons |>
    Seq.map ~f:(tag_arg_def jmp caller) |>
    Seq.fold ~init:blk ~f:inserter |>
    Term.update blk_t sub in
  let inputs id vs  =
    let vs = nullify_outputs ivars vs in
    seed_call Out prepend_def return id vs in
  let outputs id vs =
    let vs = nullify_inputs ivars vs in
    let self _ _ = Program.parent jmp_t prog (Term.tid jmp) in
    seed_call In append_def self id vs in
  match pat with
  | Pat.Call (id,v,vs) ->
    let seed f s =
      Option.value ~default:sub (f id (v::vs) s) in
    seed inputs sub |> seed outputs
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
      let vars = Defn.ivars defn in
      let cons = Defn.constrs defn in
      Defn.rules defn |>
      List.fold ~init ~f:(fun init rule ->
          let rules = Rule.premises rule @
                      Rule.conclusions rule in
          List.fold rules ~init ~f:(fun init rule -> f cons vars init rule)))

let seed_sub (spec : spec) prog sub =
  let defns = Spec.defns spec in
  let sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:sub ~f:(fun sub blk ->
        Term.enum def_t blk |>
        Seq.fold ~init:blk ~f:(fun blk def ->
            fold_patts defns ~init:blk ~f:(seed_def def)) |>
        Term.update blk_t sub) in
  Term.enum blk_t sub |>
  Seq.fold ~init:sub ~f:(fun sub blk ->
      Term.enum jmp_t blk |>
      Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_patts defns ~init:sub ~f:(seed_jmp prog jmp)))

let run spec prog =
  Term.map sub_t prog ~f:(seed_sub spec prog)
