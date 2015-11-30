open Core_kernel.Std
open Bap.Std

let fix_sp arch sub =
  let module Target = (val target_of_arch arch) in
  let width = Size.to_bits (Arch.addr_size arch) in
  (* TODO Def.create (Target.CPU.sp)... *)
  let make_addr x = Addr.of_int64 ~width x in
  let stack_offset = 0x40000000L in
  let sp_base = make_addr stack_offset in
  let first_blk = Term.first blk_t sub |> Util.val_exn in
  let mod_blk = Term.prepend def_t first_blk
      (Def.create (AMD64.CPU.sp) (Bil.int sp_base)) in
  Term.map blk_t sub ~f:(fun blk ->
    if Term.name blk = Term.name first_blk then
      mod_blk else blk)

(** Needs to be fixed. If you see Not_found it's because Sub.ssa
    fails. No idea why. *)
let propagate_consts sub =
  let (!) = Exp.fixpoint Exp.fold_consts in
  let propagate sub =
    Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
        Term.enum def_t blk |> Seq.fold ~init:sub ~f:(fun sub def ->
            let lhs = Bil.var (Def.lhs def) in
            let rhs = !(Def.rhs def) in
            match rhs with (* if after !ing we have a const, substitute *)
            | Bil.Int _ ->
              Term.map blk_t sub ~f:(fun blk ->
                  let res =
                    Blk.substitute blk lhs rhs in
                  res)
            | _ -> sub)) in
  (* after substitution, simplify everything we can in the sub *)
  let simplify sub = Term.map blk_t sub ~f:(fun blk ->
      (*Format.printf "Problem block: %s\n%!" @@ Blk.to_string blk;*)
      Blk.map_exp blk ~f:(fun e ->
          (*Format.printf "doing: e: %s!\n%!" @@ Exp.to_string e;*)
          let res = !e in
          (*Format.printf "DONE\n%!";*)
          res)) in
  let step sub = sub |> propagate |> simplify in
  let rec fp sub =
    let sub' = step sub in
    if Sub.equal sub sub' then sub else fp sub' in
  (*Format.printf "This sub will fail to SSA:\n%s\n" @@ Sub.to_string sub;*)
  try
    fp (Sub.ssa sub)
  with
  | Not_found -> failwith "Sub could not be ssa'd"

let analyze ?(fixsp=true) arch sub =
  if fixsp then
    fix_sp arch sub |> propagate_consts
  else
    propagate_consts sub
