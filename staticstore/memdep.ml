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

module Memory(Target : Target) = struct
  module G = Graphlib.Make(Int)(Label)
  module Ctxt = Target.CPU

  type t = G.t

  let add_edge g src dst lbl =
    let x, y = Var.(version src, version dst) in
    G.Edge.(insert (create x y lbl) g)

  let insert_phis g blk =
    Term.enum phi_t blk |> Seq.fold ~init:g ~f:(fun g phi ->
        if Ctxt.is_mem (Phi.lhs phi) then
          Phi.values phi |> Seq.fold ~init:g ~f:(fun g (_,src) ->
              match src with
              | Bil.Var src -> add_edge g src (Phi.lhs phi) None
              | _ -> g)
        else g)

  let insert_defs g blk =
    Term.enum def_t blk |> Seq.fold ~init:g ~f:(fun g def ->
        if Ctxt.is_mem (Def.lhs def) then
          match Def.rhs def with
          | Bil.Store (Bil.Var src,addr,data,_end,_size) ->
            add_edge g src (Def.lhs def) (Some {addr; data})
          | _ -> g
        else g)

  let create sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:G.empty ~f:(fun g blk ->
        insert_phis (insert_defs g blk) blk)

  let tainting = function
    | Bil.Int _  -> false
    | _ -> true

  let protected _ = false

  let lookup g mem addr =
    let visited = Int.Hash_set.create () in
    let rec search node =
      if Hash_set.mem visited node then None
      else
        let () = Hash_set.add visited node in
        match Seq.to_list_rev (G.Node.inputs node g) with
        | [] -> None
        | [x] -> single x
        | x :: xs -> multi x xs
    and single edge = match G.Edge.label edge with
      | Some c when Exp.equal c.addr addr -> Some c.data
      | Some {addr=a} when tainting a && not (protected a) -> None
      | _ -> search (G.Edge.src edge)
    and multi edge edges =
      let x = single edge in
      let xs = List.map edges ~f:(single) in
      if List.for_all xs ~f:(Option.equal Exp.equal x)
      then x else None in
    search (Var.version mem)

end


let memdep_of_sub proj name =
  let arch = Project.arch proj in
  let module Target = (val target_of_arch arch) in
  let module Memory = Memory(Target) in
  Term.enum sub_t (Project.program proj) |>
  Seq.find ~f:(fun sub -> Sub.name sub = name) |> function
  | None -> None
  | Some sub -> Some (Memory.create (Sub.ssa sub))


(* let Some g = *)
(*   let proj = Project.from_file "strconcat" |> ok_exn in *)
(*   memdep_of_sub proj "strcpy" *)
