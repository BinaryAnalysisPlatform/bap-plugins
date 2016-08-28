open Core_kernel.Std
open Bap.Std
open Format

open Flag

let to_csv ?dirname ~invoker data_type step_count data =
  let prefix =
    Sexp.to_string (Hook.sexp_of_t invoker)
    |> String.uncapitalize in
  match dirname with
  | Some dirname ->
    let filename =
      (match data_type,invoker with
       | `Regs,_ -> "%s_regs%04d.txt"
       | `Memory,_ -> "%s_memory%04d.txt"
       | `Path_counts,_ ->"%s_path_count%04d.txt"
       | `Trace,_ -> "%s_trace%04d.txt"
       | `Checkpoints,_ -> "%s_checkpoints%04d.txt"
       | `Myself,invoker -> "%s_self%04d.txt"
       | `Freed_addrs,_ -> "%s_freed_addrs%04d.txt"
       | `Alloced_addrs,_ -> "%s_alloced_addrs%04d.txt")
      |> fun x ->
      dirname^"/"^(sprintf x prefix step_count) in
    Output.to_csv ~filename data
  | None -> Output.to_csv data

let to_dot ?pc ?dirname sub step_count trace =
  match dirname with
  | None -> failwith "Please specify a directory with -dir option"
  | Some dirname ->
    let filename = dirname^"/"^(sprintf "cfg_frame%04d.dot" step_count) in
    Draw.save_cfg ?pc ~filename sub trace;
