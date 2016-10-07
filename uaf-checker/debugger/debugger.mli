open Bap.Std
open Microx.Std
open Concretizer
open Flag

(** TODO: split base_context and into expi_ and biri_ contexts, then
    propagate expi_ to concretizer and biri_ to conqueror *)

(** The base_context is for biri, since we are using the trace part of
     it. Also, conqueror uses the next/set_next part of it. It could
     be split to a expi_base_context and biri_base_context, but that
     would make things tricky.*)
class base_context :
  ?dir:string ->
  ?directives:Flag.t list ->
  program term -> object('s)
    inherit Biri.context

    method cstack : sub term list
    method addrs : word list
    method blk_count : int
    method path_count : int
    method branch_count : int
    method step_count : int

    method add_addr : word -> 's

    method update_callstack_enter : sub term -> 's
    method update_callstack_leave : sub term -> 's

    method inc_blk_count : 's
    method inc_step_count : 's

    method log : extras:string list -> Flag.hook -> unit
  end

(** Helper debugger classes. The conqueror class uses base_context *)
(** TODO: add path_terminates callback *)
class conqueror_debugger_context :
  ?dir:string ->
  ?directives:Flag.t list ->
  ?max_steps:int ->
  ?max_loop:int ->
  program term ->
  object('s)
    inherit Conqueror.context
    inherit base_context

    method merge_counts : 's -> 's
    method update_path_count : 's
  end

(** Debuggers do not need options! options are triggered in the debug
    l    context *)
class ['a] concretizer_debugger :
  ?memory:(addr -> word option) ->
  ?lookup:(var -> word option) ->
  ?random_seed:int ->
  ?reg_policy:Concretizer.policy ->
  ?mem_policy:Concretizer.policy ->
  project ->
  object('s)
    (*concretizer debugger is lifted to Biri.context, not
      Expi.context, since base_context must derive from Biri.context.
      Ideally, this should use a expi_debug_context.*)
    constraint 'a = #base_context
    inherit ['a] Concretizer.main
  end

class ['a] conqueror_debugger :
  ?deterministic:bool ->
  project ->
  object('s)
    constraint 'a = #conqueror_debugger_context
    inherit ['a] Conqueror.main
  end

(** TODO: the right context for conqueror_concretizer_debugger is
    conqueror_debugger_context, which doesn't feel right *)
(** TODO: should O be giving memory_lookup and register_lookup? *)
class ['a] conqueror_concretizer_debugger :
  ?deterministic:bool ->
  ?memory:(addr -> word option) ->
  ?lookup:(var -> word option) ->
  ?random_seed:int ->
  ?reg_policy:Concretizer.policy ->
  ?mem_policy:Concretizer.policy ->
  project ->
  object('s)
    constraint 'a = #conqueror_debugger_context
    inherit ['a] conqueror_debugger
    inherit ['a] concretizer_debugger
  end
