open Core_kernel.Std
open Bap.Std

class ['a] jmp : object
  constraint 'a = #Taint.context
  constraint 'a = #Biri.context
  inherit ['a] biri
end
