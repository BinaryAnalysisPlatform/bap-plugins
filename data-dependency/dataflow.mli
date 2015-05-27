open Core_kernel.Std
open Bap.Std

type direction = Forwards | Backwards

module Address : sig
  type t

  (* Create an instance of Dataflow.Address *)
  val create : addr -> int -> t

  (* Gets the assembly instruction addr from an Address.t *)
  val mem : t -> addr

  (* Gets the BIL instruction index from an Address.t *)
  val idx : t -> int

  val pp : Format.formatter -> t -> unit

  (* Return the BIL statement corresponding to an Address.t given disassembly *)
  val to_bil : t -> disasm -> stmt option

  (* Finds BIL instructions that satisfy some conditions given a list of instructions *)
  val find_bil : (mem * insn) list -> f:(stmt -> bool) -> (t * stmt) list

  include Comparable with type t:=t
  include Hashable with type t:=t
end

type dataflow

class ['a] visitor : object
  inherit [Address.t * dataflow * 'a * Domain.t] Bil.visitor
end

(* Create a dataflow analysis with the given parameters *)
val create :
  entry:block ->
  bound:mem ->
  interior:Domain.t ->
  boundary:Domain.t ->
  direction:direction -> dataflow

val run :
  dataflow ->
  worklist:block list option->
  meet:(Domain.t -> Domain.t -> Domain.t) ->
  user_state: 'a ->
  transfer:(Address.t * dataflow * 'a * Domain.t) Bil.visitor -> unit

(* Return the lattice result (list of addresses) corresponding to a block from
 * the dataflow analysis *)
val get_block : dataflow -> block -> Address.t list

(* Return the lattice result (list of addresses) corresponding to an addr
 * (instruction) from the dataflow analysis *)
val get_insn : dataflow -> addr -> Address.t list

(* Return the lattice result (list of addresses) corresponding to a BIL
 * statement from the dataflow analysis *)
val get_bil : dataflow -> Address.t -> Address.t list

(* Return all BIL instructions for which we calculated a lattice (sorted) *)
val get_all_bils : dataflow -> Address.t list

(* The following functions interface with the internal representation of the
 * dataflow analysis, and are only useful for implementing the meet and
 * transfer functions *)
(* Internal: Gets the domain value associated with a single address. *)
val find : dataflow -> Address.t -> Domain.t

(* Internal: Get the address associated with a domain value. *)
val reverse : dataflow -> Domain.t -> Address.t

(* Internal: Get the lattice value associated with a block. *)
val find_block : dataflow -> block -> Domain.t option

(* Internal: Set the lattice value associated with a block. Returns false if
 * value has not been modified, true if the value has been modified. *)
val set_block : dataflow -> block -> Domain.t -> bool

(* Internal: Get the lattice value associated with an assembly instruction. *)
val find_addr : dataflow -> addr -> Domain.t option

(* Internal: Set the lattice value associated with an assembly instruction. *)
val set_addr : dataflow -> addr -> Domain.t -> unit

(* Internal: Get the lattice value associated with a BIL statement. *)
val find_bil : dataflow -> Address.t -> Domain.t option

(* Internal: Set the lattice value associated with a BIL statement. *)
val set_bil : dataflow -> Address.t -> Domain.t -> unit

(* Return the memory associated with a dataflow address *)
val mem_from_dataflow_addr : disasm -> Address.t -> mem
