open Core_kernel.Std
open Graphlib.Std
open Bap.Std
open Format
open Or_error

let separators = [';' ]

type point = addr
type route = {
  src : point;
  checkpoints : point list;
  dst : point;
}

module Cfg = Graphs.Cfg

(** transform an address to a name of the symbol to which it
    belongs if possible, otherwise just to string *)
let string_of_point project point =
  let symbols = Project.symbols project in
  Symtab.find_by_start symbols point |> function
  | Some (name,_,_) -> name
  | _ -> asprintf "%a" Addr.pp point

(** [touches point block] true if block ends up with a call to
    [point] *)
let touches point block =
  Insn.bil (Block.terminator block) |>
  List.exists ~f:(Stmt.exists (object
                    inherit [unit] Stmt.finder
                    method enter_int target search =
                      if in_jmp && target = point then
                        search.return (Some ());
                      search
                  end))


let dfs cfg blk =
  Graphlib.postorder_traverse (module Graphs.Cfg) cfg ~start:blk

(** [goto src addr] searches from [src] for a block with a given
    [addr] *)
let goto cfg src point : block option =
  dfs cfg src |> Seq.find ~f:(fun blk -> Addr.(point = Block.addr blk))

(** [didn't_pass src dst point] if there is a path from [src] to [dst]
    that doesn't pass through the [point]. The algorithm assumes, that
    [dfs] alorithm linearize block in an execution order. In that
    case, if [dst] is reached before [point] it is impossible for the
    point to be evaluated before [dst].
    @pre [dst] is reachable from [src].
*)
let didn't_pass cfg src dst point =
  dfs cfg src |> Seq.find ~f:(fun blk ->
      Addr.(point = Block.addr blk) ||
      Block.equal blk dst) |> function
  | None -> assert false (* the dst is reachable  *)
  | Some stop -> Block.(stop = dst || stop = src)
(* the latter clause needs some justification: since dfs starts from
   the [src] block, including it, and the source block can be the
   entry block of a checkpoint, then we will stop immediately. But,
   since this entry block already touches the src, that means that
   entry occurs before src, that's imply that the control flow can't
   visit the entry block which is the checkpoint, and that means that
   it is missed. *)


(** [check_route route src] if [route.dst] is reachable from [src]
    then find checkpoint that that is not visited on path from [src]
    to [route.dst]
    @pre [blk] touches [route.src] *)
let check_route cfg route src =
  match goto cfg src route.dst with
  | None -> printf "[PASS]: since not reachable@."; None
  | Some dst -> List.find route.checkpoints ~f:(didn't_pass cfg src dst)

let print_unsafe_route p route blk missed =
  let s = string_of_point p in
  printf "[FAIL]: %s@@%a violates %s -> %s -> %s@."
    (s (Block.addr blk)) Addr.pp (Block.addr blk)
    (s route.src) (s missed) (s route.dst)

(** [check project block route] take the whole program cfg and for
    block, that starts the route checks, that the route is safe, i.e.,
    for each checkpoint there exists a path that passes through it.
    The order of checkpoints is irrelevant. Also, it is not
    guaranteed that there exists a path, that includes all
    checkpoints. *)
let check p cfg route =
  Cfg.nodes cfg |> Seq.iter ~f:(fun blk ->
      if touches route.src blk
      then match check_route cfg route blk with
        | Some missed ->
          print_unsafe_route p route blk missed
        | None -> printf "[PASS]: all checkpoints were met@.")

let addr arch x =
  let width = Arch.addr_size arch |> Size.in_bits in
  Addr.of_int64 ~width x

let make_points p s =
  if s.[0] = '0'
  then return ([addr (Project.arch p) (Int64.of_string s)])
  else
    let matches = Re.execp (Re.compile (Re_posix.re s)) in
    let symbols = Project.symbols p in
    Symtab.to_sequence symbols |> Seq.filter_map ~f:(fun (sym,blk,cfg) ->
        Option.some_if (matches sym) (Block.addr blk))
    |> Seq.to_list |> return

let create_routes ss cs ds =
  List.cartesian_product ss ds |> List.map ~f:(fun (src,dst) ->
      {src; checkpoints=cs; dst})

let parse_routes p parts =
  List.filter parts ~f:(fun p -> not(String.is_empty p)) |>
  List.map ~f:String.strip |> function
  | [] -> errorf "empty input"
  | src :: xs -> match List.rev xs with
    | [] -> errorf "route must contain 2 or more points"
    | dst :: xs ->
      make_points p src >>= fun src ->
      List.map xs ~f:(make_points p) |> all >>= fun checkpoints ->
      make_points p dst >>= fun dst ->
      match src,List.concat checkpoints,dst with
      | [],_,_ | _,_,[] -> return None
      | src, points, dst -> return (Some (create_routes src points dst))

let print_prompt chan =
  if phys_equal chan stdin then
    printf "> %!"

let process p cfg chan route =
  let parts = String.split_on_chars route ~on:separators in
  match parse_routes p parts with
  | Error err -> eprintf "Bad input: %a@." Error.pp err
  | Ok None -> printf "[PASS]: due to inexistance of endpoints@."
  | Ok Some routes -> List.iter routes ~f:(check p cfg)

let check_routes p chan =
  let cfg = Disasm.cfg (Project.disasm p) in
  print_prompt chan;
  In_channel.iter_lines chan ~f:(fun route ->
      process p cfg chan route;
      print_prompt chan)

let run p = check_routes p stdin

let () = Project.register_pass' run
