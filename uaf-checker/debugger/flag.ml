let grammar () = {|
    flag    ::=  <directives> <hooks>
    directives ::= <directive_1> .. <directive_n>
    directive ::=  <memory>
                 | <regs>
                 | <path-counts>
                 | <trace>
                 | <dot>
                 | <myself>
                 | <checkpoints>
    hooks ::= <hook_1> .. <hook_n>
    hook ::=  <eval_sub>
            | <enter_term>
            | <eval_blk>
            | <eval_jmp>
            | <eval_def>
            | <undefined_var>
            | <undefined_addr>
            | <update>
            | <load>
            | <store>
            | <lookup>
            | <path_terminates>
    |}

module Directive = struct
  type t = [
    | `Memory
    | `Regs
    | `Path_counts
    | `Trace
    | `Dot
    | `Myself (* Usually the arguments supplied to the callback. *)
    | `Checkpoints
    | `Freed_addrs
    | `Alloced_addrs
  ] [@@deriving sexp,variants]
end

type directive = Directive.t

module Hook = struct
  type t =  [
    | `Enter_term
    | `Eval_sub
    | `Eval_blk
    | `Eval_jmp
    | `Eval_def
    | `Undefined_var
    | `Undefined_addr
    | `Update
    | `Load
    | `Store
    | `Lookup
    | `Path_terminates (* Invoked right after a path terminates *)
  ] [@@deriving sexp]
end

type hook = Hook.t

type t = Flag of directive * hook list
