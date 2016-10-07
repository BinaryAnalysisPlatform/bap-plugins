open Core_kernel.Std
open Bap.Std
open Spec.Language
open Spec

let maybe_checked name =
  define ("ERR/"^name^"_maybe_checked") [
    rule "if_some_jmp_depends"
      [p := call name[_']]
      [case c jmp _']
  ] vars [reg p; reg c] such that [c/p]

let data_sanitized src san sink =
  define ("BYPASS/data_may_passthrough_"^san^"_before_"^sink) [
    rule ("if_data_passthrough_"^san)
      [sub src[p;_';_';_']; sub sink[t]]
      [sub san[s;r]]
  ] vars [reg *p; reg *t; reg *r; reg *s] such
    that [s/p; t/r; t/p]

let untrusted_input src sink =
  define ("TAINT/"^src^"_may_leak_into_"^sink) [
    rule ("if_there_is_data_dependency")
      [sub src[p]; sub sink[q]]
      [never]
  ] vars [reg *p; reg *q] such that [q/p;]

let magic source is_magic =
  define ("MAGIC/magic_door_via_"^source^"_may_exist") [
    rule ("when_magic_meets_"^source) [
      p := use v;
      sub source[_';x;_'];
      case c jmp _'
    ][never]
  ] vars [reg *x; reg c; reg p; reg v] such
    that [
    forall v such that is_magic;
    c / x;
    c / p;
  ]

let magic_may_leak_into n is_magic sink args =
  define ("MAGIC/magic_may_leak_into_"^sink^"_"^n) [
    rule "when_there_is_data_dependency"
      [p := use v; sub sink args]
      [never]
  ] vars [reg p; reg x; reg v] such
    that [forall v such that is_magic; x/p]


let escape = "_ZN7OpenDBX4Conn6escapeERKSsRSs"
let append_s = "_ZNSs6appendERKSs"
let append_n = "_ZNSs6appendEPKcj"
let create = "_ZN7OpenDBX4Conn6createERKSsNS_4Stmt4TypeE"

let unescaped_sql append =
  define ("TAINT/"^append^"_may_spoil_input") [
    rule "and_leak_into_stmt"
      [sub append[p;_']; sub create[_';_';q]]
      [never]
  ] vars [reg *p; reg *q] such that [q/p;]

let magic_leaks_into_malloc =
  define "MAGIC/magic_leaks" [
    rule "when_leaks"
      [p := use v; sub "malloc"[q]]
      [never]
  ] vars [reg p; reg q; reg v] such
    that [forall v such that is_black; q/p]

let recv_to x x_args =
  define ("TAINT/recv_to_"^x) [
    rule "if_data_dep"
      [sub "recv" [_';p;_';_']; sub x x_args]
      [never]
  ] vars [reg *p; reg *q] such that [q/p]


let unsafe_to_strcpy cat untrusted =
  define (cat^"/"^untrusted^"_to_strcpy") [
    rule "if_leaks"
      [p := call untrusted []; sub "strcpy"[q]] [never]
  ] vars [reg *p; reg *q] such that [q/p]

(* a function that returns a string without any bound
   so the only safe way to handle it, is to use strlen.*)
let returns_dynamic_string cat untrusted =
  define (cat^"/"^untrusted^"-must-be-strlened") [
    rule "if_leaks"
      [p := call untrusted []] [sub "strlen"[q]]
  ] vars [reg p; reg q] such that [q/p]



let spec = specification [
    unescaped_sql append_n;
    unescaped_sql append_s;
    maybe_checked "malloc";
    maybe_checked "calloc";
    untrusted_input "strcpy" "system";
    untrusted_input "sprintf" "system";
    untrusted_input "snprintf" "system";
    recv_to "strcpy"  [_';q];
    data_sanitized "fgets" "realpath" "fopen";
  ]
