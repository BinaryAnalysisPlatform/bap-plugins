open Bap.Std
open Core_kernel.Std

type arg = tid * Def.t

type args = {arg1 : arg option;
             arg2 : arg option;
             arg3 : arg option;
             arg4 : arg option}

(* Can be cdecl or stdcall. This code is a placeholder, and should not
   be used. *)
let to_reg_x86 = function
  | "ret"  -> IA32.CPU.rax
  | "arg1" -> IA32.CPU.rbp (*-4*)
  | "arg2" -> IA32.CPU.rbp (*-8*)
  | "arg3" -> IA32.CPU.rbp (*-12*)
  | "arg4" -> IA32.CPU.rbp (*-16*)
  | _ ->
    failwith "Arg not supported [x86]: Use [ret | arg1 | arg2 | arg3 | arg4]"

let to_reg_x86_64 = function
  | "arg1" -> AMD64.CPU.rdi
  | "arg2" -> AMD64.CPU.rsi
  | "arg3" -> AMD64.CPU.rdx
  | "arg4" -> AMD64.CPU.rcx
  | _ -> failwith "Arg not supported [x64]: Use [arg1 | arg2 | arg3 | arg4]"

let to_reg_arm = function
  | "ret"  -> ARM.CPU.r0
  | "arg1" -> ARM.CPU.r0
  | "arg2" -> ARM.CPU.r1
  | "arg3" -> ARM.CPU.r2
  | "arg4" -> ARM.CPU.r3
  | _ ->
    failwith "Arg not supported [ARM]: Use [ret | arg1 | arg2 | arg3 | arg4]"

let to_reg project arg =
  match Project.arch project with
  | `x86 -> to_reg_x86 arg
  | `x86_64 -> to_reg_x86_64 arg
  | `armv4 | `armv5 | `armv6 | `armv7
  | `armv4eb | `armv5eb | `armv6eb | `armv7eb -> to_reg_arm arg
  | arch -> failwith @@ Format.sprintf "Arch not supported: %s"
    @@ Arch.to_string arch

let infer_arg project blk arg : arg option =
  Blk.elts blk |>
  Seq.fold ~init:Seq.empty ~f:(fun acc elt ->
      match elt with
      | `Phi _ -> acc
      | `Jmp _ -> acc
      | `Def d ->
        let (!) = Var.name in
        let r = to_reg project arg in
        if !(Def.lhs d) =  !r then
          ((Term.tid d),d) ^:: acc else acc) |>
  Seq.hd

(** return a record containing all inferred args *)
let infer_args project blk =
  let f = infer_arg project blk in
  let args = Array.map [|"arg1"; "arg2"; "arg3"; "arg4"|] ~f in
  {arg1 = (args.(0));
   arg2 = (args.(1));
   arg3 = (args.(2));
   arg4 = (args.(3))}
