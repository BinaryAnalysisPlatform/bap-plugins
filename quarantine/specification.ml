open Core_kernel.Std
open Bap.Std
open Spec.Language
open Spec

let maybe_checked name =
  define (name^"_maybe_checked") [
    rule "if_some_jmp_depends"
      [p := call name[_']]
      [case c jmp _']
  ] vars [reg p; reg c] such that [c/p]

let data_sanitized src san sink =
  define ("data_may_passthrough_"^san^"_before_"^sink) [
    rule ("if_"^src^"_and_"^sink^"_exists")
      [sub src[p]]
      [sub sink[q]];
    rule ("if_data_passthrough_"^san)
      [sub src[p]; sub sink[t]]
      [sub san[s;r]]
  ] vars [reg *p; reg *q; reg *t; reg *r; reg *s] such
    that [s/p; t/r]

let untrusted_input src sink =
  define (src^"_may_leak_into_"^sink) [
    rule ("if_there_is_data_dependency")
      [sub src[p]]
      [sub sink[q]]
  ] vars [reg *p; reg *q] such that [q/p;]

let magic source is_magic =
  define "magic_door_exists" [
    rule "when_magic_meets_user_input" [
      p := use v;
      x := call source[]
    ][case c jmp d]
  ] vars [reg v; reg x; reg c; reg p] such
    that [
    forall v such that is_magic;
    c / x;
    c / p;
  ]

let spec = specification [
    maybe_checked "malloc";
    maybe_checked "calloc";
    untrusted_input "fgets" "fopen";
    data_sanitized "fgets" "realpath" "fopen";
    data_sanitized "append" "escape" "create";
  ]
