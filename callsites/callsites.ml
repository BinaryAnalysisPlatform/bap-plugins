open Core_kernel.Std
open Bap.Std

module Cmdline = struct
  open Cmdliner

  let sink : string list Term.t =
    let doc = "emit traceback if symbol matches $(docv)" in
    Arg.(value & opt_all string [] &
         info ["sink"] ~doc ~docv:"REGEX")

  let length : int Term.t =
    let doc = "Maximum length of a callsite" in
    Arg.(value & opt int 10 & info ["length"] ~doc)

  let process_args sinks length =
    let is_interesting = List.map sinks ~f:Re_posix.re
                         |> Re.alt
                         |> Re.compile
                         |> Re.execp in
    is_interesting, length

  let info =
    let doc =
      "For each sink, specifed in the command line \
       print a backtrace bound with the specified length.\
       Sinks can be specified using POSIX regular expressions" in
    Term.info ~doc "callsites"

  let parse argv =
    Term.eval ~argv (Term.(pure process_args $sink $length), info)
    |> function
    | `Ok x -> x
    | _ -> exit 1
end

module BS = Block.Set

type callstring = block list with compare, sexp_of

let callstring_of_sexp _ = failwith "callstring_of_sexp"

module CSS = Comparable.Make(struct
    type t = callstring with compare,sexp
  end)

let singleton = CSS.Set.singleton

let string_of_site syms cs =
  let addr = Block.addr cs in
  match Table.find_addr syms addr with
  | Some (mem,sym) ->
    let off = Addr.(addr - Memory.min_addr mem) in
    if Addr.is_zero off then sym
    else sprintf "%s:%s" sym Addr.(string_of_value off)
  | None -> sprintf "void:%s" Addr.(string_of_value addr)

let sym_of_site syms cs =
  let addr = Block.addr cs in
  match Symtab.find_by_start syms addr with
  | Some fn ->
    let mem = Symtab.entry_of_fn fn |> Block.memory in
    let sym = Symtab.name_of_fn fn in
    let off = Addr.(addr - Memory.min_addr mem) in
    if Addr.is_zero off then sym
    else sprintf "%s" sym
  | None -> sprintf "spontaneous"

let find_starting_with css c' =
  Set.fold css ~init:(0,[]) ~f:(fun (n',cs') cs ->
      let cs = List.rev cs in
      match List.drop_while cs ~f:(fun c -> Block.(c <> c')) with
      | [] -> n', cs'
      | cs ->
        let n = List.length cs in
        if n > n' then n,cs else n',cs') |> function
  | (_,[]) -> None
  | (_,xs) -> Some xs

let (++) = Set.union

let dedup =
  List.remove_consecutive_duplicates ~equal:String.equal

let squash = function
  | [] -> []
  | x :: xs ->
    let pps p ps =
      let ps = match List.rev ps with
        | [] -> String.Set.empty
        | x :: xs -> Set.remove (String.Set.of_list xs) x in
      sprintf "%s[%s]" p (String.concat ~sep:" " (Set.elements ps)) in
    let yield what state = Seq.Step.Yield (what,state) in
    Sequence.unfold_step ~init:(x,xs) ~f:(fun (x,xs) ->
        match x,xs with
        | [],[] -> Seq.Step.Done
        | _, [] :: _ | [], _ -> assert false
        | [x],[] -> yield x ([],[])
        | p :: ps, [] -> yield (pps p ps) ([],[])
        | [p], [jmp] :: xs -> yield p ([jmp],xs)
        | p::ps, [jmp] :: xs when p = jmp ->
          Seq.Step.Skip (p::ps,xs)
        | p::ps, [jmp] :: xs -> yield (pps p ps) ([jmp],xs)
        | [jmp], (q::qs) :: xs when q = jmp ->
          Seq.Step.Skip (q::qs, xs)
        | [jmp], (q::qs) :: xs -> yield jmp ((q::qs),xs)
        | p::ps, (q::qs) :: xs when p = q ->
          assert (ps <> []);
          assert (qs <> []);
          Seq.Step.Skip (p::(ps @ qs), xs)
        | p::ps, (q::qs) :: xs ->
          yield (pps p ps) (q::qs, xs)) |>
    Seq.to_list_rev

let main argv p =
  let is_interesting,k = Cmdline.parse argv in
  let symbols = Project.symbols p in
  let rec callstrings init n history dst : CSS.Set.t =
    match Symtab.find_by_start symbols (Block.addr dst) with
    | None -> Set.add init history
    | Some fn ->
      let bound = unstage (Symtab.create_bound symbols fn) in
      Seq.fold (Block.preds dst) ~init:(Set.add init history)
        ~f:(fun css src -> match find_starting_with css src with
            | Some cs -> Set.add css (List.rev_append cs @@ history)
            | None -> if n >= k then Set.add css history else
                let n = if bound (Block.addr src)
                  then n else n + 1 in
                callstrings css n (src :: history) src) in
  Symtab.to_sequence symbols |>
  Seq.fold ~init:CSS.Set.empty  ~f:(fun css fn ->
      let sym = Symtab.name_of_fn fn in
      let entry = Symtab.entry_of_fn fn in
      if is_interesting sym then
        let css' = callstrings css 0 [] entry  in
        Set.map (Set.diff css' css) ~comparator:String.comparator
          ~f:(fun css ->
              sprintf "%s" @@
              String.concat ~sep:" " @@
              List.remove_consecutive_duplicates
                ~equal:String.equal @@
              squash @@
              List.map ~f:(List.map ~f:(sym_of_site symbols)) @@
              List.map ~f:(fun blk ->
                  match Block.dests blk |> Seq.to_list with
                  | [`Block (call, `Jump); `Block (_,`Fall)]
                  | [`Block (_, `Fall); `Block (call, `Jump)] ->
                    [blk; call]
                  | _ -> [blk]) @@
              css) |>
        Set.elements |>
        List.remove_consecutive_duplicates ~equal:(fun x y ->
            String.is_prefix y ~prefix:x) |>
        List.iter ~f:(printf "(%s (%s))\n%!" sym);
        css'
      else css) |> ignore


let () = Project.register_pass_with_args' "callsites" main
