open Bap.Std
open Spec

let spec = [
  Definition.({
      name = "malloc_is_safe";
      judgements = [
        Judgement.({
            name = "when_checked";
            premises = [
              Rule.({
                  pat = Pat.call "malloc" [] [E.Reg "p"];
                  constr = [
                    Constr.var "p" ARM.CPU.r0;
                    Constr.int "p" (Word.of_int32 0x0l);
                  ]
                })
            ];
            conclusion = [
              Rule.({
                  pat = Pat.jump `jmp "c" "dst";
                  constr = [Constr.dep "c" "p"];
                })
            ];
          })
      ]
    })
]
