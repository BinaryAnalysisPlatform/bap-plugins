open Core_kernel.Std
open Bap.Std
open ARM.CPU
open Spec.Language


let spec = [
  define "malloc_is_safe"
    [c/p; p = r0] [
    rule "if_some_jmp_depends"
      [p := sub "malloc" []]
      [case c jmp x]
  ];

  define "malloc_is_safe_and_used"
    [c/p; p = r0] [
    rule "if_some_jmp_depeds"
      [p := sub "malloc" []; use t]
      []
  ];

  define "magic_door_exists" [
    such v that "is_magic";
    x = r0;
    c / x;
    c / p;
  ][
    rule "when_magic_meets_user_input" [
      p := term v;
      x := sub "read" []
    ][
      case c jmp d
    ]
  ];

  define "sql_exec_is_safe" [
    p = r0; p/x; y/x; p/z
  ][
    rule "if_escaped_before_exec" [
      x := term u;
      call "sql_exec"[p]
    ][
      z := sub "sql_escape"[y]
    ]
  ]
]
