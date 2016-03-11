open Bap.Std

class ['a] main :
  ?memory:(addr -> word option) ->
  ?const:word -> unit -> ['a] expi
