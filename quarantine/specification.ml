open Core_kernel.Std
open Bap.Std
open ARM.CPU
open Spec.Language


let maybe_checked name =
  define (sprintf "%s_maybe_checked" name) [
    rule "if_some_jmp_depends"
      [reg p := sub name []]
      [case c jmp x]
  ] [c/p; p = r0]



let realpath source sink =
  define "realpath_maybe_used" [
    rule "if_needed"
      [reg *p := sub source[]]
      [call sink[reg *q]];
    rule "if_data_passthrough"
      [reg *p := sub source[]; call sink[reg *t]]
      [reg *r := sub "realpath"[reg *s]]
  ] [s/p; t/r; q/r; p=r0; t=r0; r=r1; s=r0; q=r0]

let spec = [
  maybe_checked "malloc";
  maybe_checked "calloc";
  realpath "fgets" "fopen";

  define "malloc_is_safe_and_used" [
    rule "if_used_and_some_jmp_depends"
      [reg p := sub "malloc" []; any t]
      [case c jmp x]
  ] [c/p; t/p; p = r0];

  define "magic_door_exists" [
    rule "when_magic_meets_user_input" [
      reg p := use(reg v);
      reg x := sub "read" []
    ][
      case c jmp d
    ]
  ][
    such v that is_black;
    x = r0;
    c / x;
    c / p;
  ];

  define "sql_exec_is_safe" [
    rule "if_escaped_before_exec" [
      reg x := use(reg u);
      call "sql_exec"[reg p]
    ][
      reg z := sub "sql_escape"[reg y]
    ]
  ] [p = r0; p/x; y/x; p/z; such u that is_black]
]
