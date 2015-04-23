open Core_kernel.Std
open Bap.Std
open Project
open Format
open Or_error

let separators = [' '; '\t'; ','; ';' ]

type point = addr
type route = {
  src : point;
  checkpoints : point list;
  dst : point;
}

(** transform an address to a name of the symbol to which it
    belongs if possible, otherwise just to string *)
let string_of_point project point =
  Table.find_addr project.symbols point |> function
  | Some (_,sym) -> sym
  | _ -> asprintf "%a" Addr.pp point

(** [touches point block] true if block ends up with a call to
    [point] *)
let touches point block =
  Bil.find (object
    inherit [unit] Bil.finder
    method enter_int target search =
      if in_jmp && target = point then
        search.return (Some ());
      search
  end) (Insn.bil (Block.terminator block))


(** [goto src addr] searches from [src] for a block with a given
    [addr] *)
let goto src point : block option =
  Block.dfs src |> Seq.find ~f:(fun blk -> Addr.(point = Block.addr blk))

(** [didn't_pass src dst point] if there is a path from [src] to [dst]
    that doesn't pass through the [point]. The algorithm assumes, that
    [dfs] alorithm linearize block in an execution order. In that
    case, if [dst] is reached before [point] it is impossible for the
    point to be evaluated before [dst].
    @pre [dst] is reachable from [src].
*)
let didn't_pass src dst point =
  Block.dfs src |> Seq.find ~f:(fun blk ->
      Addr.(point = Block.addr blk) ||
      Block.equal blk dst) |> function
  | None -> assert false (* the dst is reachable  *)
  | Some stop -> Block.(stop = dst)

(** [check_route route src] if [route.dst] is reachable from [src]
    then find checkpoint that that is not visited on path from [src]
    to [route.dst]
    @pre [blk] touches [route.src] *)
let check_route route src =
  match goto src route.dst with
  | None -> None
  | Some dst -> List.find route.checkpoints ~f:(didn't_pass src dst)

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
let check p blocks route =
  Table.iter blocks ~f:(fun blk ->
      if touches route.src blk
      then match check_route route blk with
        | Some missed ->
          print_unsafe_route p route blk missed
        | None -> ())

let addr arch x =
  let width = Arch.addr_size arch |> Size.to_bits in
  Addr.of_int64 ~width x

let make_point p s =
  if s.[0] = '0'
  then return (Some (addr p.arch (Int64.of_string s)))
  else Table.find_mapi p.symbols ~f:(fun mem sym ->
      Option.some_if (sym = s) (Memory.min_addr mem)) |> return

let create_route p parts =
  List.filter parts ~f:(fun p -> not(String.is_empty p)) |> function
  | [] -> errorf "empty input"
  | src :: xs -> match List.rev xs with
    | [] -> errorf "route must contain 2 or more points"
    | dst :: xs ->
      make_point p src >>= fun src ->
      List.map xs ~f:(make_point p) |> all >>= fun checkpoints ->
      make_point p dst >>= fun dst ->
      match src,checkpoints,dst with
      | None,_,_ | _,_,None -> return `No_such_route
      | Some src,[],Some dst -> return (`Route {src; dst; checkpoints=[]})
      | Some src, points, Some dst ->
        let missedpoints,checkpoints =
          List.fold2_exn xs points ~init:([],[])
            ~f:(fun (ms,ps) name -> function
                | Some p -> ms,p::ps
                | None -> name::ms,ps) in
        match missedpoints with
        | [] ->  return (`Route {src; checkpoints; dst})
        | points -> return (`Missed points)

let print_prompt chan =
  if phys_equal chan stdin then
    printf "> %!"

let process p blocks chan route =
  String.split_on_chars route ~on:separators |>
  create_route p |> function
  | Ok `No_such_route -> eprintf "no such route@."
  | Ok (`Route route) -> check p blocks route
  | Ok (`Missed them) -> printf "[FAIL]: no such checkpoints: %s@."
    @@ String.concat ~sep:", " them
  | Error err -> eprintf "Bad input: %a@." Error.pp err

let check_routes p chan =
  let blocks = Disasm.blocks p.disasm in
  print_prompt chan;
  In_channel.iter_lines chan ~f:(fun route ->
      process p blocks chan route;
      print_prompt chan)

let run p = check_routes p stdin

let () = register_plugin' run