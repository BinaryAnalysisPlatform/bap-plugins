open Core_kernel.Std
open Bap.Std
open Bil.Result
open Spec


type t = tid

type set = Tid.Set.t with bin_io, compare, sexp
type map = set Var.Map.t with bin_io, compare, sexp


(** value stored in register is source of taint  *)
val reg : t tag

(** value stored at memory location, that is stored
    in the register is tainted.*)
val ptr : t tag

val regs : map tag

val ptrs : map tag

val merge : map -> map -> map

class context :  object('s)
  method taint_reg : Bil.result -> set -> 's
  method taint_ptr : addr -> size -> set -> 's
  method reg_taints : Bil.result -> set
  method ptr_taints : addr -> set
  method all_taints : set
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


val pp_set : Format.formatter -> set -> unit

val pp_map : Format.formatter -> map -> unit

module Map : Regular with type t = map
