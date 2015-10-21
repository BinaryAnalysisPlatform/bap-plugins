open Core_kernel.Std
open Bap.Std

class ['a] with_jmp : object
  constraint 'a = #Tainter.context
  constraint 'a = #Biri.context
  inherit ['a] biri
end
