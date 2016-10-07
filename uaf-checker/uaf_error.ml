open Core_kernel.Std
open Bap.Std
open Format
open Options

type t = {
  trace : tid list;
  addr : addr;
  use_tid : tid;
  free_tid : tid;
  alloc_tid : tid;
}

let callstack prog error =
  let trace = error.trace in
  List.fold ~init:[] trace ~f:(fun acc tid ->
      (match Program.lookup sub_t prog tid with
       | Some sub -> (Sub.name sub) :: acc
       | None -> acc) |> fun acc ->
      if Tid.(error.alloc_tid = tid) then "~ALLOC~" :: acc
      else if Tid.(error.free_tid = tid) then "~FREE~" :: acc
      else if Tid.(error.use_tid = tid) then "~USE~" :: acc
      else acc)

let print_callstack proj options error =
  if options.verbose then
    let prog = Project.program proj in
    List.iter (callstack prog error) ~f:(fun sub -> printf "%s\n%!" sub)

let print_trace options error =
  if options.verbose then
    (printf "=-=-=-=-=-=-=-= DUMP -=-=-=-=-=-=-=-\n";
     printf "Trace\n%!";
     List.iter error.trace ~f:(fun tid -> printf "%a@." Tid.pp tid))

(** Does not print trace by default *)
let pp ppf e =
  fprintf ppf
    "UAF detected!@.\
     Address: %a@.\
     Used at: %a@.\
     Free'd at: %a@.\
     Alloc'd at: %a@."
    Addr.pp e.addr
    Tid.pp e.use_tid
    Tid.pp e.free_tid
    Tid.pp e.alloc_tid

let to_string e =
  asprintf "%a" pp e

let pps () e = to_string e
