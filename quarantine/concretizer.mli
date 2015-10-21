open Core_kernel.Std
open Bap.Std

type summary = (var * Bil.value) list


class ['a] t :
  ?summary:(call -> summary option) ->
  ?mapping:(addr -> tid option) ->
  ?const:word -> unit -> ['a] biri


(** a bag of tricks  *)
module Tricks : sig
  class ['a] constant : word -> ['a] expi
  class ['a] indirects_from_mapping : (word -> tid option) -> ['a] biri
  class ['a] shortcut_indirect_calls : ['a] biri
  class ['a] summarize_calls : (call -> summary option) -> ['a] biri
end
