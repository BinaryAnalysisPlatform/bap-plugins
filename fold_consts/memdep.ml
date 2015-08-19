open Core_kernel.Std
open Bap.Std
open Utils

type change = {addr : exp; data : exp} with compare
type range = Set of Exp.Set.t | All with compare
type label =
  | Nothing
  | Change of change
  | Clobber of range
with compare

type value =
  | Bot                       (* nothing *)
  | Val of exp                (* constant *)
  | Top                       (* any *)
with compare

module Label = struct
  type t = label with compare
  include Opaque.Make(struct
      type t = label with compare
      let hash = function
        | Change c -> Exp.hash c.addr lxor Exp.hash c.data
        | other -> Hashtbl.hash other
    end)
end

module G = Graphlib.Make(Int)(Label)

type t = {
  arch : arch;
  graph : G.t;
  const : unit memmap;
  mutab : unit memmap;
}

let empty arch = {
  arch;
  graph = G.empty;
  const = Memmap.empty;
  mutab = Memmap.empty;
}


let in_range range exp = match range with
  | Set s -> Set.mem s exp
  | All -> true

let add_edge m src dst lbl =
  let x, y = Var.(version src, version dst) in
  {m with graph = G.Edge.(insert (create x y lbl) m.graph)}

let insert_phis is_mem m blk =
  Term.enum phi_t blk |> Seq.fold ~init:m ~f:(fun m phi ->
      if is_mem (Phi.lhs phi) then
        Phi.values phi |> Seq.fold ~init:m ~f:(fun m (_,src) ->
            match src with
            | Bil.Var src -> add_edge m src (Phi.lhs phi) Nothing
            | _ -> m)
      else m)

let insert_defs is_mem g blk =
  Term.enum def_t blk |> Seq.fold ~init:g ~f:(fun g def ->
      if is_mem (Def.lhs def) then
        match Def.rhs def with
        | Bil.Store (Bil.Var src,addr,data,_end,_size) ->
          add_edge g src (Def.lhs def) (Change {addr; data})
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

let protected _ = false

let taints write_addr read_addr = match write_addr with
  | Bil.Int _  -> false
  | _ -> true

let join x xs =
  List.fold_left xs ~init:x ~f:(fun x y -> match x,y with
      | Bot,_ | _,Bot -> Bot
      | Top,x | x,Top -> x
      | Val x, Val y -> if Exp.(x = y) then Val x else Bot)

let lookup_graph m var addr =
  let visited = Int.Hash_set.create () in
  let rec search node =
    match Seq.to_list_rev (G.Node.inputs node m.graph) with
    | [] -> Top
    | [x] -> single x
    | x :: xs -> multi x xs
  and single edge =
    let next = G.Edge.src edge in
    match G.Edge.label edge with
    | Change c when Exp.equal c.addr addr -> Val c.data
    | Change {addr=a} when taints a addr -> Bot
    | Clobber s when in_range s addr -> Bot
    | lbl when Hash_set.mem visited next -> Top
    | _ -> Hash_set.add visited next; search next
  and multi edge edges =
    join (single edge) (List.map edges ~f:single) in
  match search (Var.version var) with
  | Val x -> Some x
  | _ -> None


(* todo: instead of using default word size, we can try to infer
   argument type, based on generated dependency graph. *)
let lookup_const m addr =
  match addr with
  | Bil.Int addr ->
    let scale = mem_elt_size m.arch in
    Memmap.lookup m.const addr |> Seq.find_map ~f:(fun (mem,_) ->
        match Memory.get ~scale ~addr mem with
        | Error _ -> None
        | Ok word -> Some (Bil.Int word))
  | _ -> None


let lookup m var addr =
  match lookup_const m addr with
  | None -> lookup_graph m var addr
  | x -> x
