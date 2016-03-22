open Core_kernel.Std
open Bap.Std
open X86_cpu

let mem_summary_to_string (src,addr) =
  Format.sprintf "[%s]:[%s]" (Exp.to_string src) (Exp.to_string addr)

let print_store_map =
  Def.Map.iteri ~f:(fun ~key ~data ->
      Format.printf "K: %sD: %s\n"
        (Def.to_string key) (mem_summary_to_string data))

(* We might see:
   000000f4: t_167 := mem64[0x601050:64, el]:u8*)
let mem_read_to_var exp =
  (object inherit Bil.mapper
    method! map_load ~mem ~addr endian size =
      match addr with
      | Bil.Int addr ->
        let var_name = Format.sprintf "m_%x" (Word.to_int addr |> ok_exn) in
        Bil.var @@ Var.create var_name reg32_t (** TODO architecture agnostic*)
      | exp ->
        (* binop mapper so we hit every instance of a mem read in an
           expression *)
        (object inherit Bil.mapper
          method! map_binop op o1 o2 =
            let orig = Bil.binop op o1 o2 in
            match (op,o1,o2) with
            | (Bil.PLUS, Bil.Var v, Bil.Int off)
            | (Bil.MINUS, Bil.Var v, Bil.Int off) ->
              if AMD64.is_bp v || AMD64.is_sp v || ARM.CPU.is_bp v || ARM.CPU.is_sp v then (* skips FS_BASE *)
                let offset =
                  Word.to_int off |> ok_exn in
                let var_name =
                  Format.sprintf "%s_%04x" (Exp.to_string o1) offset in
                Bil.var @@ Var.create var_name reg32_t
              else
                orig
            | _ -> orig
        end)#map_exp exp
  end)#map_exp exp

let mem_access_to_var exp =
  (object inherit [Var.t] Exp.visitor
    method! enter_var v state =
      (** only if we haven't already visited it in binop *)
      (** TODO: should this check is_bp / is_sp? *)
      if Var.name state = "initial" then
        let vname = Var.name v in
        Var.create (vname^"_0") reg32_t
      else
        state
    method! enter_binop op o1 o2 state =
      match (op,o1,o2) with
      | (Bil.PLUS, Bil.Var v, Bil.Int off)
      | (Bil.MINUS, Bil.Var v, Bil.Int off) ->
        if AMD64.is_bp v || AMD64.is_sp v || ARM.CPU.is_sp v || ARM.CPU.is_bp v then (* skips FS_BASE *)
          let offset =
            Word.to_int off |> ok_exn in
          let var_name = Format.sprintf "%s_%04x" (Exp.to_string o1) offset in
          Var.create  var_name reg32_t
        else
          state
      | _ -> state
  end)#visit_exp exp (Var.create "initial" reg32_t)

(** stores all the defs which perform a store (write) operation on
    memory. This is important if we want to restrict ourselves to
    only definitions which write ONCE to memory, and thereafter is
    always read. *)
let make_store_map ?(v=false) sub =
  Term.enum blk_t sub |> Seq.fold ~init:Def.Map.empty ~f:(fun map blk ->
      Term.enum def_t blk |> Seq.fold ~init:map ~f:(fun map def ->
          match Var.typ (Def.lhs def) with
          | Type.Imm _ -> map (* either immeidate of n bits or mem *)
          | Type.Mem _ ->
            match Def.rhs def with
            | Bil.Store (dst,src,addr,endian,size) ->
              if Def.Map.exists map ~f:(fun (src',_) -> src' = src)
              then if v then
                  Format.printf "EXISTS: %s!\n Def: %s"
                    (Exp.to_string src) (Def.to_string def);
              Def.Map.add map ~key:def ~data:(src,addr)
            | _ -> map))

let replace_stores ?(v=false) sub =
  let res_store_map = make_store_map ~v sub in
  if v then print_store_map res_store_map;

  (** Only transfrom definitions in the store map *)
  Term.map blk_t sub ~f:(fun blk ->
      Def.Map.fold res_store_map ~init:blk
        ~f:(fun ~key:def ~data:(mem_access,value_stored) blk' ->
            (* convert src to a variable if it is reg +- offset *)
            let replacement_var = mem_access_to_var mem_access in
            match Var.name replacement_var with
            | "initial" -> blk' (* dont do it *)
            | _ ->
              let replacement_def =
                Def.create ~tid:(Term.tid def) replacement_var value_stored in
              Term.update def_t blk' replacement_def))

(** Casts can happen on [bp + offset], so we should replace those with
    the variable *)
let replace_loads sub =
  Term.map blk_t sub ~f:(Blk.map_exp ~f:mem_read_to_var)

let mem_to_reg ?(v=false) sub =
  sub |> replace_stores ~v |> replace_loads

let analyze ?(v=false) sub =
  mem_to_reg ~v sub
