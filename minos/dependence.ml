open Bap.Std
open Core_kernel.Std
open Options
open Check

let (^::) = Seq.cons


let print_seq s =
  Format.printf "Defs:";
  Seq.iter s ~f:(fun x -> Format.printf "%a " Def.pp x)

let debug s acc def =
  Format.printf s;
  print_seq acc

let get_vars exp =
  (object inherit [Var.t seq] Exp.visitor
    method! enter_var v state =
      v ^:: state
  end)#visit_exp exp

(** TODO: improve this. We don't need SSA *)
let defs_from_vars defs sub =
  let all_defs =
    Term.enum blk_t sub
    |> Seq.map ~f:(fun blk -> Term.enum def_t blk)
    |> Seq.concat in
  Seq.filter_map defs ~f:(fun v ->
      (* for each block in sub, if it contains a def lhs that
         matches var name, then make that the def *)
      Seq.find_map all_defs ~f:(fun def ->
          let ssa_name = Format.sprintf "%a" Var.pps in
          if ssa_name @@ Def.lhs def = ssa_name v then
            Some def else None))

(** Add data dependencies of def tid, if this tid exists *)
let def_dep sub tid =
  (*Format.printf "Dep: %s" @@ Tid.to_string tid;*)
  let all_defs =
    Term.enum blk_t sub
    |> Seq.map ~f:(fun blk -> Term.enum def_t blk)
    |> Seq.concat in
  let first_def = Seq.find all_defs ~f:(fun def ->
      if Term.tid def = tid then
        true else false) in
  match first_def with
  | Some first_def ->
    let rec add_defs acc def =
      (*Format.printf "Adding: %s" @@ Def.to_string def;*)
      let acc = def ^:: acc in
      let vars = get_vars (Def.rhs def) Seq.empty in
      match Seq.length vars with
      | 0 -> acc
      | _ ->
        let defs = defs_from_vars vars sub in
        Seq.fold defs ~init:acc ~f:add_defs in
    add_defs Seq.empty first_def
  | None -> Seq.empty

(** Add data dependencies of jmp tid, if this tid exists *)
let jmp_dep sub tid =
  let all_jmps =
    Term.enum blk_t sub
    |> Seq.map ~f:(fun blk -> Term.enum jmp_t blk)
    |> Seq.concat in
  let first_jmp = Seq.find all_jmps ~f:(fun jmp ->
      if Term.tid jmp = tid then
        true else false) in
  match first_jmp with
  | Some jmp ->
    let first_vars_of_jmp =
      get_vars (Jmp.cond jmp) Seq.empty in
    let first_defs = defs_from_vars first_vars_of_jmp sub in
    let rec add_defs acc def =
      let acc = def ^:: acc in
      let vars = get_vars (Def.rhs def) Seq.empty in
      match Seq.length vars with
      | 0 -> acc
      | _ ->
        let defs = defs_from_vars vars sub in
        Seq.fold defs ~init:acc ~f:add_defs in
    Seq.fold first_defs ~init:Seq.empty ~f:add_defs
  | None -> Seq.empty


(** Get the dependence information for a tid. It can be either a def
    or jmp term *)
(*
let run sub tid =
  match (def_dep sub tid, jmp_dep sub tid) with
  | (s1,s2) when Seq.is_empty s1 -> s2
  | (s1,s2) when Seq.is_empty s2 -> s1
  | _ -> Seq.empty
*)

let strip =
  String.filter ~f:(function
      | '\n' -> false
      | _ -> true)

let inter_dep deps1 deps2 =
  Tid.Set.of_list (Seq.to_list deps1) |>
  Tid.Set.inter (Tid.Set.of_list (Seq.to_list deps2)) |>
  Tid.Set.to_sequence

let highlight_cli ?(highlight=[]) (ctxt : Check.ctxt) sub dependence =
  let open Color in
  let output = "" in
  let no = !!Brown in (* normal, actually *)
  let to_string_def def =
    Format.sprintf "%a" Def.pps def |> strip in
  let to_string_jmp jmp =
    Format.sprintf "%a" Jmp.pps jmp |> strip in
  let s = Format.sprintf "Path %d\n=========\n" ctxt.count in
  let output = output^s in

  let output =
    Term.enum blk_t sub |>
    Seq.fold ~init:output ~f:(fun output blk ->
        let prelude = Format.sprintf "%s:\n"
            (Term.tid blk |> Tid.name |> String.chop_prefix_exn ~prefix:"%") in
        let output = output^prelude in

        let output =
          Blk.elts blk |>
          Seq.fold ~init:output ~f:(fun output elt ->
              match elt with
              | `Def def ->
                (match List.Assoc.find highlight (Term.tid def) with
                 | Some color ->
                   let s = Format.sprintf "%s\n" (color^to_string_def def^no) in
                   output^s
                 | None ->
                   let s = Format.sprintf "%s\n" @@ to_string_def def in
                   output^s)
              | `Jmp jmp ->
                (match List.Assoc.find highlight (Term.tid jmp) with
                 | Some color ->
                   let s =  Format.sprintf "%s\n" (color^to_string_jmp jmp^no) in
                   output^s
                 | None ->
                   let s = Format.sprintf "%s\n" @@ to_string_jmp jmp in
                   output^s)
              | _ -> failwith "elt is not a def or blk") in
        output^"\n") in
  if ctxt.options.verbose then Format.printf "%s" output;
  Output.path ctxt.path_dir ctxt.count output

let output sub' arg_dependence jmp_dependence sink_intersect_dependence jmp_tids
    (ctxt : Check.ctxt) =
  let open Color in
  let add_color = List.Assoc.add in
  let add_aqua l tid = add_color l tid !!Aqua in
  let add_red l tid = add_color l tid !!Red in
  let add_green l tid = add_color l tid !!Green in
  let r seq f init = Seq.fold ~init seq ~f in
  let highlight =
    [] |> r arg_dependence add_aqua |>
    r jmp_dependence add_red |>
    r jmp_tids add_red |>
    r sink_intersect_dependence add_green in

  (*match Seq.length arg_dependence with
    | 0 -> ()
    | _ ->
    (* let sub' = Resolve_calls.resolve_calls ctxt.project sub' in *)*)
  highlight_cli ~highlight ctxt sub' arg_dependence
