open Core_kernel.Std
open Bap.Std
open ARM.CPU
open Spec.Language


let maybe_checked name =
  define (name^"_maybe_checked") [
    rule "if_some_jmp_depends"
      [p := sub name []]
      [case c jmp x]
  ] vars [reg p; reg c] such that [c/p; p = r0]

let data_sanitized src san sink =
  define ("data_may_passthrough_"^san^"_before_"^sink) [
    rule ("if_"^src^"_and_"^sink^"_exists")
      [p := sub src[]]
      [call sink[q]];
    rule ("if_data_passthrough_"^san)
      [p := sub src[]; call sink[t]]
      [r := sub san[s]]
  ] vars [reg *p; reg *q; reg *t; reg *r; reg *s] such
    that [s/p; t/r; q/r; p=r0; t=r0; r=r1; s=r0; q=r0]

let untrusted_input src sink =
  define (src^"_may_leak_into_"^sink) [
    rule ("as_"^sink^"_depends_on_"^src)
      [p := sub src[]]
      [call sink[q]]
  ] vars [reg p; reg q] such that [q/p; p=r0; q=r0]

let magic source is_magic =
  define "magic_door_exists" [
    rule "when_magic_meets_user_input" [
      p := use v;
      x := sub source []
    ][case c jmp d]
  ] vars [reg v; reg x; reg c; reg p] such
    that [
    forall v such that is_magic;
    x = r0;
    c / x;
    c / p;
  ]



let spec = [
  maybe_checked "malloc";
  maybe_checked "calloc";
  untrusted_input "fgets" "fopen";
  data_sanitized "fgets" "realpath" "fopen";
  data_sanitized "append" "escape" "create";
]
