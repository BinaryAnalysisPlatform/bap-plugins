open Bap.Std
open Spec

let spec = [
  Definition.({
      name = "malloc_is_safe";
      constrs = [
        Constr.var "p" ARM.CPU.r0;
        Constr.int "p" (Word.of_int32 0x0l);
        Constr.dep "c" "p"
      ];
      rules = [
        Rule.({
            name = "when_checked";
            premises = [Pat.call "malloc" [] [E.Reg "p"]];
            conclusions = [Pat.jump `jmp "c" "dst"];
          })
      ]
    })
]
