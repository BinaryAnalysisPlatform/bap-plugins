open Core_kernel.Std
open Bap.Std

type priority = int

let def_of_tid blk tid =
  let all_defs =
    Term.enum def_t blk in
  Seq.find all_defs ~f:(fun def ->
      if Term.tid def = tid then
        true else false) |> function
  | Some def -> def
  | None -> failwith "no def for this tid"

let blk_contains_tid blk tid =
  Option.is_some (Term.find def_t blk tid)

let dep_blk_span dep sub_path =
  Term.enum blk_t sub_path |> Seq.fold ~init:0 ~f:(fun acc blk ->
      if Seq.exists dep ~f:(fun tid -> blk_contains_tid blk tid) then acc+1
      else acc)

let get_rodata_str project w =
  let open Or_error in
  match Util.find_section_by_name project ".rodata" with
  | Some mem -> if Memory.contains mem w then
      let mem' = Memory.view ~word_size:`r8 ~from:w mem |> Or_error.ok_exn in
      let _,res =
        let (!) = Char.to_string in
        Memory.foldi ~word_size:`r8 mem' ~init:(false,"")
          ~f:(fun addr word (set,acc) ->
              let char = Word.enum_chars word LittleEndian |> Seq.hd_exn in
              match set,char with
              | (false,'\x00') -> (true,acc)
              | (false,c) -> (false,acc^(!c))
              | (true,c) -> (true,acc)) in res |> Option.some
    else None
  | None -> None

(** if it's in .rodata but hidden by a layer of indirection through
    .text *)
let try_get_rodata project w =
  let open Or_error in
  (*let size = Project.arch...*)
  match Util.find_section_by_name project ".text" with
  | Some mem ->
    (match Memory.get ~scale:`r32 ~addr:w mem with
     | Ok w -> get_rodata_str project w
     | Error e -> None)
  | None -> None

let get_arg_as_string project blk def_tid =
  match Def.rhs (def_of_tid blk def_tid) with
  | Bil.Int w ->
    (match get_rodata_str project w with
     | Some str -> Some str
     | None -> try_get_rodata project w)
  | Bil.Load (_,Bil.Int w,_,_) ->
    try_get_rodata project w
  | _ -> None

(** TODO: ok_exn will bite you *)
let get_arg_as_const project blk def_tid =
  match Def.rhs (def_of_tid blk def_tid) with
    Bil.Int w ->  Word.to_int w |> ok_exn |> Option.some
  | _ -> None

module Predicate = struct

  let arg_is_const_0 blk def_tid =
    match Def.rhs (def_of_tid blk def_tid) with
    | Bil.Int w when w = Word.b0 -> true
    | _ -> false

  let arg_is_const blk def_tid =
    match Def.rhs (def_of_tid blk def_tid) with
    | Bil.Int w -> true
    | _ -> false

  let arg_is_not_const blk def_tid =
    match Def.rhs (def_of_tid blk def_tid) with
    | Bil.Int w -> false
    | _ -> true

  let contains_call call_name sub =
    Util.calls_of_sub sub |> List.map ~f:Tid.name |>
    List.exists ~f:(fun call_name' -> call_name = call_name')

  let contains_calls call_names sub =
    Util.calls_of_sub sub |> List.map ~f:Tid.name |>
    List.exists ~f:(fun call_name' ->
        List.exists call_names ~f:(fun call_name -> call_name = call_name'))

  let c_ite exp =
    (object inherit [bool] Exp.visitor
      method! enter_ite ~cond ~yes ~no state = true || state
    end)#visit_exp exp false

  let contains_ite sub s =
    let defs =
      Term.enum blk_t sub
      |> Seq.map ~f:(fun blk -> Term.enum def_t blk)
      |> Seq.concat in
    Seq.exists defs ~f:(fun def -> c_ite @@ Def.rhs def)

  let in_rodata project w =
    match Util.find_section_by_name project ".rodata" with
    | Some mem -> Memory.contains mem w
    | None -> false

  let arg_is_rodata project blk def_tid =
    match Def.rhs (def_of_tid blk def_tid) with
    | Bil.Int w -> in_rodata project w
    | _ -> false

  let arg_is_not_rodata project blk def_tid =
    not (arg_is_rodata project blk def_tid)

  let arg_rodata_contains project blk def_tid substring =
    match Def.rhs (def_of_tid blk def_tid) with
    | Bil.Int w -> let rodata_str = get_rodata_str project w in
      (match rodata_str with
       | Some str ->
         Format.printf "Debug: retreived %s\n%!" str;
         String.is_substring ~substring str
       | None -> false)
    | _ -> false

  let arg_is_string project blk def_tid =
    Option.is_some @@ get_arg_as_string project blk def_tid

  let arg_is_not_string project blk def_tid =
    not (arg_is_string project blk def_tid)

  let arg_string_contains project blk def_tid substring =
    match get_arg_as_string project blk def_tid with
    | Some str -> String.is_substring ~substring str
    | None -> false

  let size_gt seq i = Seq.length seq > i

  let size_lt seq i = Seq.length seq < i

  let size_eq seq i = Seq.length seq = i

  let size_eq_0 seq = size_eq seq 0

  let size_eq_1 seq = size_eq seq 1
end
