open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std
open Utils

(** This data type is used to represent update of smallest possible
    chunk of memory (a byte).
    {addr;value} updates a byte at given [addr] with a given value.
    Invariant: typeof(value) = reg8_t *)
type update = {
  base : exp;
  disp : size option;           (* zero (None) or up to size *)
  data : exp
} [@@deriving compare]

(** abstract value as an element of a flat lattice, where
    flat lattice over set [S] is defined as
    [L = <{⊤, ⊥} ∪ S, ∨ >], where [x ∨ y], aka «meet» returns
    greatest lower bound of [x] and [y]. [meet x y = x] iff
    [x] is syntactically equal to [y].

    Note: derived [compare] is a regular structural comparison,
    not the lattice one.
*)
type 'a value = Bot | Val of 'a | Top
[@@deriving compare]

(** a contiguous memory chunk (a word)  *)
type chunk = exp value array [@@deriving compare]

(** label of a dependency graph  *)
type label = update list
[@@deriving compare]

let option_hash hash = function
  | None -> Hashtbl.hash None
  | Some x -> hash x

module Label = struct
  type t = label [@@deriving compare]
  include Opaque.Make(struct
      type t = label [@@deriving compare]
      let hash =
        List.fold ~init:(Hashtbl.hash []) ~f:(fun h x ->
            h
            lxor option_hash Size.hash x.disp
            lxor Exp.hash x.base
            lxor Exp.hash x.data)
    end)
end

module G = Graphlib.Make(Int)(Label)

type t = {
  arch : arch;
  graph : G.t;
  const : unit memmap;
}

let empty arch = {
  arch;
  graph = G.empty;
  const = Memmap.empty;
}

let meet x y = match x,y with
  | Bot,_ | _,Bot -> Bot
  | Top,x | x,Top -> x
  | Val x, Val y -> if Exp.(x = y) then Val x else Bot

let add_edge m src dst lbl =
  let x, y = Var.(index src, index dst) in
  {m with graph = G.Edge.(insert (create x y lbl) m.graph)}

let insert_phis is_mem m blk =
  Term.enum phi_t blk |> Seq.fold ~init:m ~f:(fun m phi ->
      if is_mem (Phi.lhs phi) then
        Phi.values phi |> Seq.fold ~init:m ~f:(fun m (_,src) ->
            match src with
            | Bil.Var src -> add_edge m src (Phi.lhs phi) []
            | _ -> m)
      else m)

let enum_word endian word = Word.enum_bytes word endian

let enum_exp size endian = function
  | Bil.Int word ->
    let bytes = enum_word endian word |> Seq.map ~f:Bil.int in
    Seq.take bytes (Size.in_bytes size) |> Seq.to_list
  | exp ->
    let bytes = Seq.init (Size.in_bytes size) ~f:(fun n ->
        Bil.extract ~lo:(n*8) ~hi:((n+1)*8 - 1) exp) in
    match endian with
    | LittleEndian -> Seq.to_list bytes
    | BigEndian -> Seq.to_list_rev bytes

let insert_defs is_mem g blk =
  Term.enum def_t blk |> Seq.fold ~init:g ~f:(fun g def ->
      if is_mem (Def.lhs def) then
        match Def.rhs def with
        | Bil.Store (Bil.Var src,addr,data,e,sz) ->
          let updates =
            enum_exp sz e data |> List.mapi ~f:(fun n data -> {
                  base = addr;
                  disp = Size.of_int_opt (n * 8);
                  data
                }) in
          add_edge g src (Def.lhs def) updates
        | _ -> g
      else g)

let create_const memory =
  Memmap.to_sequence memory |>
  Seq.fold ~init:Memmap.empty ~f:(fun consts (mem,tag) ->
      match Value.get Image.segment tag with
      | Some seg when Image.Segment.is_writable seg -> consts
      | Some seg when Image.Segment.is_readable seg ->
        Memmap.add consts mem ()
      | _ -> consts)

let create ?(memory = Memmap.empty) arch sub =
  let module Target = (val target_of_arch arch) in
  let is_mem = Target.CPU.is_mem in
  let const = create_const memory in
  let init = {(empty arch) with const} in
  Term.enum blk_t sub |>
  Seq.fold ~init ~f:(fun m blk ->
      insert_phis is_mem (insert_defs is_mem m blk) blk)

let taints write_addr read_addr = match write_addr with
  | Bil.Int _  -> false
  | _ -> true


let meet_chunk xs ys = Array.map2_exn xs ys ~f:meet
let meet_edges c cs = List.fold cs ~init:c ~f:meet_chunk


module Disp = Comparable.Make(struct
    type t = size option [@@deriving sexp, compare]
  end)

let disp_to_bytes = function
  | None -> 0
  | Some size -> Size.in_bytes size

(** [interference u a n] is
    [Some true] if update [u] changes byte at addr [a+n];
    [Some false] if it changes some other byte;
    [None] if it may change unknown address, including this.

    The result is actually meet semi-lattice over bools, with [None]
    as bottom. *)
let interference u addr n =
  if Exp.(u.base = addr)
  then Some (disp_to_bytes u.disp = n)
  else match u.base, addr with
    | (Bil.Int base, Bil.Int addr) ->
      let base = Addr.(base ++ disp_to_bytes u.disp) in
      Some Addr.(base = addr ++ n)
    | _ -> None

let update_chunk updates base chunk =
  Array.mapi chunk ~f:(fun n v -> match v with
      | Bot | Val _ -> v
      | Top -> List.find_map updates ~f:(fun u ->
          match interference u base n with
          | None -> Some Bot
          | Some false -> None
          | Some true -> Some (Val u.data)) |> function
               | None -> Top
               | Some r -> r) |> fun chunk ->
  if Array.for_all chunk ~f:(function Val _ -> true | _ -> false)
  then `Done chunk
  else `Next chunk

let init_chunk size =
  Array.init (Size.in_bytes size) ~f:(fun _ -> Top)


exception Undefined
exception Symbolic

let exp_of_chunk chunk =
  let bytes = Array.fold chunk ~init:[] ~f:(fun acc -> function
      | Top | Bot -> raise Undefined
      | Val b -> b :: acc) in
  try List.map bytes ~f:(function
      | Bil.Int x -> x
      | _ -> raise Symbolic) |> List.reduce_exn ~f:Word.concat |> Bil.int
  with Symbolic -> List.reduce_exn bytes ~f:Bil.concat

let lookup_graph m ~mem:var ~addr endian size =
  let visited = Int.Hash_set.create () in
  let rec search chunk node =
    match Seq.to_list_rev (G.Node.inputs node m.graph) with
    | [] -> chunk
    | [x] -> single chunk x
    | x :: xs -> multi chunk x xs
  and single chunk edge =
    let next = G.Edge.src edge in
    match update_chunk (G.Edge.label edge) addr chunk with
    | `Done chunk -> chunk
    | `Next chunk when Hash_set.mem visited next -> chunk
    | `Next chunk ->
      Hash_set.add visited next; search chunk next
  and multi ch edge edges =
    meet_edges (single ch edge) (List.map edges ~f:(single ch)) in
  let chunk =  search (init_chunk size) (Var.index var) in
  try Some (exp_of_chunk chunk) with Undefined -> None

(* todo: instead of using default word size, we can try to infer
   argument type, based on generated dependency graph. *)
let lookup_const m addr endian size =
  match addr with
  | Bil.Int addr ->
    Memmap.lookup m.const addr |> Seq.find_map ~f:(fun (mem,_) ->
        match Memory.get ~scale:size ~addr mem with
        | Error _ -> None
        | Ok word -> Some (Bil.Int word))
  | _ -> None


let load m ~mem ~addr endian size =
  match lookup_const m addr endian size with
  | Some x -> Some x
  | None -> match mem with
    | Bil.Var mem -> lookup_graph m ~mem ~addr endian size
    | _ -> None
