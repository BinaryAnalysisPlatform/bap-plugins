open Core_kernel.Std
open Bap.Std

type exit_type = [`Seen | `Max_depth | `Terminal]

  (* Printers *)
(* -------- *)
(** Print calls of a blk seq path*)
let print_calls_of_blk_seq blk_seq =
  Seq.iter blk_seq ~f:(fun blk ->
      let jmps = Term.enum jmp_t blk in
      let names = Seq.map jmps ~f:Util.resolve_jmp_name in
      Seq.iter names ~f:(fun x -> match x with
          | Some x ->
            Format.printf "%s\n" x
          | None -> ()))

let print_path path =
  Format.printf "path:\n";
  Seq.iter path ~f:(fun node ->
      Format.printf "Tid: %a\n" Tid.pp node)

let print_path_content sub path =
  Format.printf "path:\n";
  Seq.iter path ~f:(fun node ->
      Format.printf "Tid: %a\n" Tid.pp node;
      let blk = Util.blk_of_tid sub node in
      Format.printf "%a\n" Blk.pp blk;
      Term.enum jmp_t blk |> Seq.iter ~f:(fun jmp ->
          let e = Jmp.cond jmp in
          Format.printf "[--Jmp cond: %a : %a--]\n" Jmp.pp jmp Exp.pp e))

(** given a path (and sub, so we can resolve tid's), print the
    things it calls *)
let print_path_calls path sub =
  print_calls_of_blk_seq (Seq.map path ~f:(fun tid -> Util.blk_of_tid sub tid))

let print_blk_path path =
  Format.printf "Blk path:\n";
  Seq.iter path ~f:(fun node ->
      Format.printf "Blk: %a" Blk.pp node)

let blks_of_path path sub =
  Seq.map path ~f:(fun tid -> Util.blk_of_tid sub tid)

(* Utils *)
(* ----- *)
let blk_seq_of_tid_seq path sub =
  Seq.map path ~f:(fun tid -> Util.blk_of_tid sub tid)

let rec fold_paths_prog ?(rev=false) ~prog ~state ~acc ~sub ~finish ~f =
  let next = if rev
    then Graphlib.Callgraph.Node.preds
    else Graphlib.Callgraph.Node.succs in
  match next sub prog with
  | l when Seq.is_empty l -> finish state sub acc
  | children ->
    Seq.fold ~init:acc children
      ~f:(fun acc child ->
          let continue = fold_paths_prog ~rev ~prog ~acc ~sub:child ~finish ~f
          in
          f state sub continue acc)

let debug succs blk =
  Format.printf "Current: %s Succs:%!" @@ Tid.to_string blk;
  Seq.iter succs ~f:(fun s ->
      Format.printf "%s| " @@ Tid.to_string s);
  Format.printf "\n%!"

let rec fold_paths_graph ?(rev=false)
    (module G : Graphlib.Graph with
      type edge = Graphlib.Tid.Tid.edge and
    type node = tid and
    type t = Graphlib.Tid.Tid.t)
    ~(sub : Graphlib.Tid.Tid.t) ~state ~acc ~(blk : tid) ~finish ~f =
  let next = if rev
    then G.Node.preds
    else G.Node.succs in
  match next blk sub with
  | l when Seq.is_empty l -> finish state blk acc
  | children ->
    Seq.fold ~init:acc children
      ~f:(fun acc child ->
          let continue = fold_paths_graph ~rev (module G) ~sub ~acc ~blk:child
              ~finish ~f in
          f state blk continue acc)

let rec fold_paths ~sub ~state ~acc ~blk ~finish ~f =
  match Util.succs_of_blk_tid sub blk with
  | l when Seq.is_empty l -> finish state blk acc
  | children ->
    Seq.fold ~init:acc children
      ~f:(fun acc child ->
          let continue = fold_paths ~sub ~acc ~blk:child ~finish ~f in
          f state blk continue acc)

(** pass the accumulator along if we terminate early *)
type 'a termination =
    Terminal of 'a
  | Seen of 'a
  | Max_depth of 'a

let calls_of_path path =
  Seq.fold ~init:0 path ~f:(fun acc blk ->
      let jmps = Term.enum jmp_t blk in
      let names = Seq.map jmps ~f:Util.resolve_jmp_name in
      Seq.fold ~init:acc (Seq.zip names jmps) ~f:(fun acc (name,jmp) ->
          match name with
          | Some name ->
            Format.printf "%s\n" name;
            acc
          | None -> acc))
