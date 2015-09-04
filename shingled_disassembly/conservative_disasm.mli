open Bap.Std
module Dis = Disasm_expert.Basic
val run : ('a, 'k) Dis.t -> Memory.t
  -> (Memory.t * (Dis.asm, Dis.kinds) Dis.insn option) list

val run_with :
  ('a, 'b) Dis.t ->
  Bap.Std.Memory.t ->
  at:(Bap.Std.Memory.t * (Dis.asm, Dis.kinds) Dis.insn option -> 'c list -> 'c list) ->
  'c list
