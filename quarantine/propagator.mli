open Core_kernel.Std
open Bap.Std
open Taint



class ['a] t : object
  constraint 'a = #context
  inherit ['a] expi
end
