open Core_kernel.Std
open Bap.Std
open Pathlib

let _v ?(v=false) ?(tag="") s =
  if v then
    Format.printf "%s %s" tag s

let already_seen seen sub_tid =
  Seq.exists seen ~f:(fun tid ->
      if sub_tid = tid then true else false)

let print_callstring ?(v=false) path =
  if v then
    (List.iter (Seq.to_list path |> List.rev) ~f:(fun tid ->
         if v then Format.printf "|%s|" @@ Tid.name tid);
     Format.printf "\n")

let print_count ?(v=false) count =
  _v ~v @@ Format.sprintf "%08d%!" count;
  _v ~v @@ Format.sprintf "\x08\x08\x08\x08\x08\x08\x08\x08%!"

(* Path search and check *)
let do_path_search
  ?(v=false) ?(rev=false) start prog max_depth =

  let process_path local_state sub_tid termination =
    let current_path,depth = local_state in
    let path = sub_tid ^:: current_path in
    print_callstring ~v path;
    match termination with
    | `Terminal (_,count) ->
      print_count ~v count;
      (path ^:: Seq.empty, 1)
    | `Seen (_,count) ->
      print_count ~v count;
      (path ^:: Seq.empty, 1)
    | `Max_depth (_,count) ->
      print_count ~v count;
      (path ^:: Seq.empty, 1)
  in

  let finish local_state sub_tid ctxt =
    let paths,count = ctxt in
    let r_path,r_count =
      process_path local_state sub_tid (`Terminal ctxt) in
    (Seq.append paths r_path, count + r_count)
  in

  let f local_state sub_tid continue ctxt =
    let paths,count = ctxt in
    let current_path,depth = local_state in
    let process = process_path local_state sub_tid in
    if already_seen current_path sub_tid then
      let r_path, r_count =
        process (`Seen ctxt) in
      (Seq.append paths r_path, count + r_count)
    else if depth = max_depth then
      let r_path, r_count = process (`Max_depth ctxt) in
      (Seq.append paths r_path, r_count + count)
    else
      begin
        let state = (sub_tid ^:: current_path, depth+1) in
        continue ~state
      end
  in

  let initial_local_state = (Seq.empty, 0) in
  let initial_ctxt = (Seq.empty, 0) in

  fold_paths_prog
    ~rev
    ~prog
    ~state:initial_local_state
    ~acc:initial_ctxt
    ~sub:(Util.tid_of_sub start) ~finish ~f
