open Core_kernel.Std
open Bap.Std
open ARM.CPU
open Spec.Language


let spec = [
  define "malloc_is_safe" [
    rule "if_some_jmp_depends"
      [p := sub "malloc" []]
      [case c jmp x]
  ] [c/p; p = r0];

  define "malloc_is_safe_and_used" [
    rule "if_used_and_some_jmp_depends"
      [p := sub "malloc" []; use t]
      [case c jmp x]
  ] [c/p; t/p; p = r0];

  define "magic_door_exists" [
    rule "when_magic_meets_user_input" [
      p := term v;
      x := sub "read" []
    ][
      case c jmp d
    ]
  ][
    such v that "is_magic";
    x = r0;
    c / x;
    c / p;
  ];

  define "sql_exec_is_safe" [
    rule "if_escaped_before_exec" [
      x := term u;
      call "sql_exec"[p]
    ][
      z := sub "sql_escape"[y]
    ]
  ] [p = r0; p/x; y/x; p/z]
]
