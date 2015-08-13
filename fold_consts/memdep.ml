open Core_kernel.Std
open Bap.Std

type change = {addr : exp; data : exp} with compare
type label = change option with compare

module Label = struct
  type t = label with compare
  include Opaque.Make(struct
      type t = label with compare
      let hash = function
        | None -> 0
        | Some c -> Exp.hash c.addr lxor Exp.hash c.data
    end)
end

module G = Graphlib.Make(Int)(Label)

type t = G.t

type value =
  | Bot                       (* nothing *)
  | Val of exp                (* constant *)
  | Top                       (* any *)

let add_edge g src dst lbl =
  let x, y = Var.(version src, version dst) in
  G.Edge.(insert (create x y lbl) g)

let insert_phis is_mem g blk =
  Term.enum phi_t blk |> Seq.fold ~init:g ~f:(fun g phi ->
      if is_mem (Phi.lhs phi) then
        Phi.values phi |> Seq.fold ~init:g ~f:(fun g (_,src) ->
            match src with
            | Bil.Var src -> add_edge g src (Phi.lhs phi) None
            | _ -> g)
      else g)

let insert_defs is_mem g blk =
  Term.enum def_t blk |> Seq.fold ~init:g ~f:(fun g def ->
      if is_mem (Def.lhs def) then
        match Def.rhs def with
        | Bil.Store (Bil.Var src,addr,data,_end,_size) ->
          add_edge g src (Def.lhs def) (Some {addr; data})
        | _ -> g
      else g)

let create is_mem sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:G.empty ~f:(fun g blk ->
      insert_phis is_mem (insert_defs is_mem g blk) blk)

let protected _ = false

let taints write_addr read_addr = match write_addr with
  | Bil.Int _  -> false
  | _ -> true

let join x xs =
  List.fold_left xs ~init:x ~f:(fun x y -> match x,y with
      | Bot,_ | _,Bot -> Bot
      | Top,x | x,Top -> x
      | Val x, Val y -> if Exp.(x = y) then Val x else Bot)

let lookup g mem addr =
  let visited = Int.Hash_set.create () in
  let rec search node =
    match Seq.to_list_rev (G.Node.inputs node g) with
    | [] -> Top
    | [x] -> single x
    | x :: xs -> multi x xs
  and single edge =
    let next = G.Edge.src edge in
    match G.Edge.label edge with
    | Some c when Exp.equal c.addr addr -> Val c.data
    | Some {addr=a} when taints a addr -> Bot
    | lbl when Hash_set.mem visited next -> Top
    | _ -> Hash_set.add visited next; search next
  and multi edge edges =
    join (single edge) (List.map edges ~f:single) in
  match search (Var.version mem) with
  | Val x -> Some x
  | _ -> None


(* let memdep_of_sub proj name = *)
(*   let arch = Project.arch proj in *)
(*   let module Target = (val target_of_arch arch) in *)
(*   let is_mem = Target.CPU.is_mem in *)
(*   Term.enum sub_t (Project.program proj) |> *)
(*   Seq.find ~f:(fun sub -> Sub.name sub = name) |> function *)
(*   | None -> None *)
(*   | Some sub -> Some (create is_mem (Sub.ssa sub)) *)


(* let Some g = *)
(*   let proj = Project.from_file "strconcat" |> ok_exn in *)
(*   memdep_of_sub proj "strcpy" *)
