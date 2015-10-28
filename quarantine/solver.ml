open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec

type t = spec


let create = ident

let seed = Value.Tag.register
    ~name:"seed"
    ~uuid:"1ab9a363-db8f-4ab4-9fb4-5ff54de97c5c"
    (module Tid)

let input_of_constr = function
  | Constr.Dep (_,v) -> Some v
  | _ -> None

let inputs_of_rule rule =
  Rule.constr rule |>
  List.filter_map  ~f:input_of_constr |>
  V.Set.of_list

let inputs_of_rules rules =
  List.map rules ~f:inputs_of_rule |>
  List.fold ~init:V.Set.empty ~f:Set.union

let inputs_of_judgement jud =
  Set.union
    (inputs_of_rules (Judgement.conclusion jud))
    (inputs_of_rules (Judgement.premises jud))

let collect_inputs spec =
  List.fold spec ~init:[] ~f:(fun init def ->
      List.fold (Definition.judgements def) ~init ~f:(fun init jud ->
          init))

let our_target id = function
  | Indirect _ -> false
  | Direct tid -> Tid.name tid = "@"^id


let unknown_of_var msg var =
  Def.create var (Bil.unknown msg (Var.typ var))

let def_of_cons v cons =
  let var = List.find_map cons ~f:(function
      | Constr.Var (v',var) when V.(v' = v) -> Some var
      | _ -> None) in
  let int = List.find_map cons ~f:(function
      | Constr.Int (v',int) when V.(v' = v) -> Some int
      | _ -> None) in
  match var,int with
  | Some x, Some w -> Some (Def.create x (Bil.int w))
  | Some x, None -> Some (unknown_of_var "undefined after call" x)
  | _ -> None

let self_seed t = Term.set_attr t seed (Term.tid t)

let insert_seeds vars cons v blk =
  if Set.mem vars v then match def_of_cons v cons with
    | None -> blk
    | Some def -> Term.prepend def_t blk (self_seed def)
  else blk


let seed_jmp jmp vars sub rule =
  let seed_call id es cons = match Jmp.kind jmp with
    | Ret _ | Int _ | Goto _ -> sub
    | Call call when not (our_target id (Call.target call)) -> sub
    | Call call ->
      List.filter es ~f:(function
          | E.Ptr {Ptr.base=b; off=d} ->
            Set.mem vars b || Set.mem vars d
          | E.Reg v -> Set.mem vars v) |> function
      | [] -> sub
      | es -> match Call.return call with
        | None | Some (Indirect _) -> sub
        | Some (Direct ret) -> match Term.find blk_t sub ret with
          | None -> sub
          | Some blk ->
            List.fold es ~init:blk ~f:(fun blk -> function
                | E.Ptr {Ptr.base=b; off=o} ->
                  insert_seeds vars cons b blk |>
                  insert_seeds vars cons o
                | E.Reg v -> insert_seeds vars cons v blk) |>
            Term.update blk_t sub in
  let cons = Rule.constr rule in
  match Rule.pat rule with
  | Pat.Call (id,e1,[]) -> sub
  | Pat.Call (id,_,es) -> seed_call id es cons
  | _ -> sub

let seed_def def vars blk rule =
  let hit = Set.mem vars in
  let hitp {Ptr.base=v1; off=v2} = hit v1 || hit v2 in
  let seed = Some (self_seed def) in
  let def = match Rule.pat rule with
    | Pat.Move (v1,v2) when hit v1 || hit v2 -> seed
    | Pat.Load (v,p)
    | Pat.Store (v,p) when hit v || hitp p -> seed
    | _ -> None in
  match def with
  | None -> blk
  | Some def -> Term.update def_t blk def

let fold_judgements spec ~init ~f =
  List.fold spec ~init ~f:(fun init defn ->
      Definition.judgements defn |>
      List.fold ~init ~f:(fun init jud ->
          let vars = inputs_of_judgement jud in
          let rules = Judgement.premises jud @
                      Judgement.conclusion jud in
          List.fold rules ~init ~f:(fun init rule -> f vars init rule)))

let seed_sub (spec : t) sub =
  Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
      let blk =
        Term.enum def_t blk |> Seq.fold ~init:blk ~f:(fun blk def ->
            fold_judgements spec ~init:blk ~f:(seed_def def)) in
      let sub = Term.update blk_t sub blk in
      Term.enum jmp_t blk |> Seq.fold ~init:sub ~f:(fun sub jmp ->
          fold_judgements spec ~init:sub ~f:(seed_jmp jmp)))

let seed_program spec prog =
  Term.map sub_t prog ~f:(seed_sub spec)
