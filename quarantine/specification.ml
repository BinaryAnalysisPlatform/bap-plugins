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
  define ("magic_door_via_"^source^"_may_exist") [
    rule ("when_magic_meets_"^source) [
      p := use v;
      sub source[_';x;_']
    ][case c jmp _']
  ] vars [reg *x; reg c; reg p; reg v] such
    that [
    forall v such that is_magic;
    c / x;
    c / p;
  ]


let magic_may_leak_into n is_magic sink args = 
  define ("magic_may_leak_into_"^sink^"_"^n) [
    rule "when_there_is_data_dependency"
      [p := use v]
      [sub sink args]
  ] vars [reg p; reg x; reg v] such
    that [forall v such that is_magic; x/p]
  

let escape = "_ZN7OpenDBX4Conn6escapeERKSsRSs"
let append_s = "_ZNSs6appendERKSs"
let append_n = "_ZNSs6appendEPKcj"
let create = "_ZN7OpenDBX4Conn6createERKSsNS_4Stmt4TypeE"

let unescaped_sql append =
  define (append^"_may_spoil_input") [
    rule "and_leak_into_stmt"
      [sub append[p;_']]
      [sub create[_';_';q]]
  ] vars [reg *p; reg *q] such that [q/p;]

let magic_leaks_into_malloc = 
  define "magic_leaks" [
    rule "when_leaks"
      [p := use v]
      [sub "malloc"[q]]
  ] vars [reg p; reg q; reg v] such
    that [forall v such that is_black; q/p]

let spec = specification [
    unescaped_sql append_n;
    unescaped_sql append_s;
    maybe_checked "malloc";
    maybe_checked "calloc";
    untrusted_input "fgets" "fopen";
    data_sanitized "fgets" "realpath" "fopen";
    magic "read" is_black;
    magic "readv" is_black;
    magic "recvmsg" is_black;
    magic_may_leak_into "1" is_black "strcmp" [_';x];
    magic_may_leak_into "2" is_black "strcmp" [x;_'];
    magic_may_leak_into "3" is_black "strncmp" [_';x];
    magic_may_leak_into "4" is_black "strncmp" [x;_'];
  ]
