Lifter Benchmark
================

This simple tool will take a file and disassemble it from the very
beginning till the last byte (even if it is not code, at all), and try
to lift every possible instruction. It won't try to parse file headers,
find code sections, or anything like this. At the end, if everything
went fine, it will print the descriptive statistics of the lifter.

The benchmark also disables the optimization pipeline, however there is still
a small overhead over the lifter, because the typechecking is enforced.
In fact, at the end, we typecheck at least trice :)

Building and using
==================

```
make
./bench.native <filename>
```

You can also specify an architecture:
```
./bench.native <arch> <filename>
```


Examples
========

On Intel(R) Xeon(R) CPU E5-2630 v4 @ 2.20GHz, bap 1.5 built wit OCaml 4.05.0+flambda,
and llvm-3.8, yields the following results,
```
$ ./bench.native /lib/x86_64-linux-gnu/libc-2.23.so
Statistics for the x86_64 lifter
Total time: 6.89342 s
Total throughtput: 265 kB/s
Insn throughtput: 74532 I/s
Insn latency: 13.42 mks/I
Bytes processed: 1868831
Data bytes: 24636
Code bytes: 1844195
Code density: 98.68%
Total number of instructions: 513777
Lifted instructions: 497837
Lifting coverage: 96.90%
```

and for Google Chrome

```
$ ./bench.native /opt/google/chrome/chrome
Statistics for the x86_64 lifter
Total time: 571.788 s
Total throughtput: 240 kB/s
Insn throughtput: 75191 I/s
Insn latency: 13.30 mks/I
Bytes processed: 140517351
Data bytes: 1285721
Code bytes: 139231630
Code density: 99.09%
Total number of instructions: 42993264
Lifted instructions: 41657415
Lifting coverage: 96.89%
```

Normalized to the CPU speed (i.e., CPU speed / throughput ), it means
that currently the lifter on average makes 9,000 ops per disassembled
byte and 30,000 operations per disassembled instruction.


Caveats
=======

For some reason the arm backend in LLVM just stops instead of erroring,
on a malformed data. So for arm we need to feed it with something that
looks like code.
