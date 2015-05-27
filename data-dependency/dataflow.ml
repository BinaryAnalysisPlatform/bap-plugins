open Core_kernel.Std
open Bap.Std
open Project
open Option

exception No_conversion

type direction = Forwards | Backwards

module Address = struct
  module T = struct
    type t = addr * int with sexp

    let hash t = (Addr.hash (fst t)) lxor (Int.hash (snd t))

    let compare t1 t2 =
      if fst t1 > fst t2 then 1
      else if fst t1 < fst t2 then -1
      else begin
        if snd t1 > snd t2 then 1
        else if snd t1 < snd t2 then -1
        else 0
      end
  end

  type t = T.t

  let create addr idx = (addr, idx)

  let mem t = fst t

  let idx t = snd t

  let pp fmt t = Format.fprintf fmt "(%a, %a)" Addr.pp (fst t) Int.pp (snd t)

  let to_bil target disasm =
    match Disasm.insn_at_addr disasm (fst target) with
    | Some (mem, insn) ->
      let (_, _, result) = Bil.fold
          ~init:((Memory.min_addr mem, 0), target, None)
          (object inherit [t * t * stmt option] Bil.visitor
            method! enter_stmt stmt (address, target, ret) =
              (address, target, if address = target then Some stmt else ret)
            method! leave_stmt _ ((addr, idx), target, result) =
              ((addr, idx + 1), target, result)
          end) (Insn.bil insn) in
      result
    | None -> None

  let find_bil insns ~f =
    List.fold ~init:[] ~f:(fun accum (mem, insn) ->
        accum @ snd @@ Bil.fold ~init:(create (Memory.min_addr mem) 0, []) (object
          inherit [t * (t * stmt) list] Bil.visitor
          method! enter_stmt stmt (address, ret) =
            match f stmt with
            | true -> (address, (address, stmt) :: ret)
            | false -> (address, ret)
          method! leave_stmt _ ((addr, idx), ret) =
            ((addr, idx + 1), ret)
        end) (Insn.bil insn)
      ) insns

  include Comparable.Make(T)
  include Hashable.Make(T)
end

type dataflow = {
  (* Internal variables for dataflow analysis *)
  mutable counter : int;
  mutable domain_map : Address.t Domain.Map.t;
  mutable address_map : Domain.t Address.Map.t;

  (* Externally available dataflow parameters *)
  addr_lattice : (addr, Domain.t) Hashtbl.t;
  bil_lattice : (Address.t, Domain.t) Hashtbl.t;
  block_lattice : (block, Domain.t) Hashtbl.t;
  interior : Domain.t;
  direction : direction;
  blocks : block seq;
}

class ['a] visitor = object
  inherit [Address.t * dataflow * 'a * Domain.t] Bil.visitor

  method! leave_stmt _ ((addr, idx), dataflow, state, accum) =
    ((addr, idx + 1), dataflow, state, accum)
end

let find dataflow address =
  match Address.Map.mem dataflow.address_map address with
  | false -> let value = Domain.create_finite [dataflow.counter] true in
    dataflow.address_map <- Address.Map.add dataflow.address_map
        ~key:address ~data:value;
    dataflow.domain_map <- Domain.Map.add dataflow.domain_map
        ~key:value ~data:address;
    dataflow.counter <- dataflow.counter + 1;
    value
  | true -> Address.Map.find_exn dataflow.address_map address

let reverse dataflow domain = Map.find_exn dataflow.domain_map domain

let find_block dataflow block = Hashtbl.find dataflow.block_lattice block

let find_addr dataflow addr = Hashtbl.find dataflow.addr_lattice addr

let find_bil dataflow stmt = Hashtbl.find dataflow.bil_lattice stmt

let set_addr dataflow addr value =
  Hashtbl.set dataflow.addr_lattice ~key:addr ~data:value

let set_block dataflow block value =
  let existing = Hashtbl.find_exn dataflow.block_lattice block in
  if Domain.(existing = value) then false else begin
    Hashtbl.set dataflow.block_lattice ~key:block ~data:value; true
  end

let set_bil dataflow stmt value =
  Hashtbl.set dataflow.bil_lattice ~key:stmt ~data:value

let create ~entry ~bound ~interior ~boundary ~direction =
  let addr_lattice = Hashtbl.create ~hashable:Addr.hashable () in
  let bil_lattice = Hashtbl.create ~hashable:Address.hashable () in
  let block_lattice = Hashtbl.create ~hashable:Block.hashable () in
  let domain_map = Domain.Map.empty in
  let address_map = Address.Map.empty in
  (* Temporary fix for BAP bug *)
  let blocks = Seq.force_eagerly @@ match direction with
    | Forwards -> Block.dfs ~bound entry
    | Backwards -> Block.dfs ~next:Block.preds ~bound entry in
  Seq.iter blocks ~f:(fun block ->
      let initial = if Block.(block = (Seq.hd_exn blocks)) then boundary else interior in
      Hashtbl.set block_lattice ~key:block ~data:initial
    );
  { counter = 0; domain_map; address_map; addr_lattice;
    bil_lattice; block_lattice; interior; direction; blocks }

let rec run dataflow ~worklist ~meet ~user_state ~transfer =
  match worklist with
  | None -> run dataflow ~worklist:(Some (Seq.to_list dataflow.blocks))
              ~meet ~user_state ~transfer
  | Some [] -> ()
  | Some list ->
    let (next_list, next_state) = List.fold list ~init:([], user_state)
        ~f:(fun (new_list, state) block ->
            let meet_in = match dataflow.direction with
              | Forwards -> Block.preds block
              | Backwards -> Block.succs block in
            let meet = Seq.fold meet_in ~init:dataflow.interior
                ~f:(fun accum block ->
                    match find_block dataflow block with
                    (* Ignore references to blocks outside the current function *)
                    | None -> accum
                    | Some block -> meet accum block) in
            let (transfer, state) = List.fold (Block.insns block) ~init:(meet, state) 
                ~f:(fun (accum, state) (mem, insn) ->
                    let addr = (Memory.min_addr mem, 0) in
                    let bil = Insn.bil insn in
                    let (_, _, new_state, result) = Bil.fold
                        ~init:(addr, dataflow, state, accum) transfer bil in
                    set_addr dataflow (fst addr) result;
                    (result, new_state)
                  ) in
            match set_block dataflow block transfer with
            | false -> (new_list, state)
            | true -> (new_list @ [block], state)
          ) in
    run dataflow ~worklist:(Some next_list) ~meet
      ~user_state:next_state ~transfer

let get_insn dataflow addr =
  match find_addr dataflow addr with
  | None -> Format.eprintf "Unknown insn: %a\n" Addr.pp addr; []
  | Some value -> Domain.fold value ~init:[] ~f:(fun accum value ->
      reverse dataflow value :: accum)

let get_block dataflow block =
  match find_block dataflow block with
  | None -> Format.eprintf "Unknown block: %a\n" Block.pp block; []
  | Some value -> Domain.fold value ~init:[] ~f:(fun accum value ->
      reverse dataflow value :: accum)

let get_all_bils dataflow =
  Hashtbl.keys dataflow.bil_lattice |> List.sort ~cmp:Address.compare

let get_bil dataflow addr =
  match find_bil dataflow addr with
  | None -> Format.eprintf "Unknown addr: %a\n" Address.pp addr; []
  | Some value -> Domain.fold value ~init:[] ~f:(fun accum value ->
      reverse dataflow value :: accum)

let mem_from_dataflow_addr disasm daddr =
  let addr = fst daddr in
  match Disasm.insn_at_addr disasm addr with
  | Some (mem,_) -> mem
  | None -> raise No_conversion
