Initial pass for resolving strings. Annotates project with the comment tag.

`bap --print-bir-attr=comment --symbols=ida -lstrings --strings -dbir /bin/ls`

```
.comment "/usr/share/locale"
000001a2: RSI := 0x41381C:64
.comment "coreutils"
000001a3: RDI := 0x413800:64
000001a4: RSP := RSP - 0x8:64
000001a5: mem64 := mem64 with [RSP, el]:u64 <- 0x40290F:64
000001a6: call @.bindtextdomain with return %000001a7
```

