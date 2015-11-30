open Bap.Std
open Core_kernel.Std

let to_reg_x86_64 = function
  | "arg1" -> "RDI"
  | "arg2" -> "RSI"
  | "arg3" -> "RDX"
  | "arg4" -> "RCX"
  | "arg5" -> "R8"
  | "arg6" -> "R9"
  | "" -> ""
  | _ ->
    failwith "Arg not supported [x64]: Use [arg1 | arg2 | arg3 | \
              arg4 | arg5 | arg6"

(* Can be cdecl or stdcall. This code is a placeholder, and should not
   be used. *)
let to_reg_x86 = function
  | "ret" -> "EAX"
  | "arg1" -> "EBP-4"
  | "arg2" -> "EBP-8"
  | "arg3" -> "EBP-12"
  | "arg4" -> "EBP-16"
  | "arg5" -> "EBP-20"
  | "arg6" -> "EBP-24"
  | "" -> ""
  | _ ->
    failwith "Arg not supported [x86]: Use [ret | arg1 | arg2 | arg3 | arg4 "

let to_reg_arm = function
  | "ret"  -> "R0"
  | "arg1" -> "R0"
  | "arg2" -> "R1"
  | "arg3" -> "R2"
  | "arg4" -> "R3"
  | "" -> ""
  | _ ->
    failwith "Arg not supported [ARM]: Use [ret | arg1 | arg2 | arg3 | arg4 "

let to_reg_name project arg =
  match Project.arch project with
  | `x86 -> to_reg_x86 arg
  | `x86_64 -> to_reg_x86_64 arg
  | `armv4 | `armv5 | `armv6 | `armv7
  | `armv4eb | `armv5eb | `armv6eb | `armv7eb -> to_reg_arm arg
  | arch -> failwith @@ Format.sprintf "Arch not supported: %s"
    @@ Arch.to_string arch

(** returns defs for arguments *)
let infer project blk arg =
  Blk.elts blk |>
  Seq.fold ~init:Seq.empty ~f:(fun acc elt ->
      match elt with
      | `Def d -> let v = Def.lhs d in
        let prefix = to_reg_name project arg in
        if String.is_prefix (Var.name v) ~prefix &&
           (not (String.is_empty prefix)) then
          ((Term.tid d), d) ^:: acc
        else
          acc
      | `Phi p ->
        acc
      | `Jmp j ->
        acc)
