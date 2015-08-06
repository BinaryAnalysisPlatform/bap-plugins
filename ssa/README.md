# Transfrom program into SSA form

This plugin will transform all subroutines in a IR of a program into
an SSA form.

# Compilation

```
$ bapbuild ssa.plugin
```

# Usage

```
$ bap <filename> -lssa
```

or to view the result:

```
$ bap <filename> -lssa -dbir
```

# Literature

    [1]: Muchnick, Advanced Compiler Design and Implementation
         [ISBN-10: 1558603204]
    [2]: Appel, Modern Compiler Implementation in ML
         [ISBN 0-521-60764-7]
    [3]: Cooper, Engineering a Compiler, Second Edition
         [ISBN-10: 012088478X]
