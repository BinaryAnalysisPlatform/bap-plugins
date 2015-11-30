open Core_kernel.Std
open Bap.Std
open Bil.Result

type t = tid
type taints = Tid.Set.t

val seed : t tag

val vars : taints Var.Map.t tag

class context :  object('s)
  method taint_val : Bil.result -> taints -> 's
  method taint_mem : addr -> size -> taints -> 's
  method val_taints : Bil.result -> taints
  method mem_taints : addr -> taints
  method taints : taints
end

(** Propagate taint through expressions.


    {2 Semantics}

    {3 Grammar}

    The following syntactic forms are used in propagation rules:

    [*a] - load from address [a], where [a] is immediate value;
    [*a <- v] - store value [v] at address [a];
    [exp ~> v] - expression reduces to value [v];
    [v -> t] - value [v] is tainted by a taint [t];
    [<bop>] - BIL binary operation or BIL concat expression;
    [<uop>] - BIL unary, extract or cast expression.


    {3 Rules}

    Value [v] is tainted by taint [t], denoted as [v -> t], if there
    exists a deriviation of the following rules, proving this fact.

    {v

    *a ~> v
    a -> t
    ---------------- :: p_load
    v -> t

    *a <- v
    v -> t
    ---------------- :: p_store
    a -> t

    v1 <bop> v2 ~> v3
    v1 -> t
    ----------------- :: p_bop_lhs
    v3 -> t

    v1 <bop> v2 ~> v3
    v2 -> t
    ----------------- :: p_bop_rhs
    v3 -> t

    <uop> v1 ~> v2
    v1 -> t
    ----------------- :: p_uop
    v2 -> t

    v}

    Note 1: this class overrides only methods, that computes non-leaf
    expressions, leaving a space for extension for derived classes.

    Note 2: we do not propagate taint from condition to branches in the
    if/then/else expression, since we're propagating only data
    dependency, not control dependency.

    Although, one can argue, that in expression [if c then x else y]
    the result depends on [c], since if we change [c] we will get
    different results, there is a good reason for not propagating this
    dependency - the consistency with BIR and BIL. Consider, BIL's
    [if] statement or BIR's conditional jump. If we will start to
    propagate taint from condition in [ite] expression, then we should
    also propagate it in BIL's and BIR's conditionals. Unfortunatelly
    the latter is not possible.

*)
class ['a] propagator : object('s)
  constraint 'a = #context
  inherit ['a] expi
end


val pp_taints : Format.formatter -> taints -> unit
