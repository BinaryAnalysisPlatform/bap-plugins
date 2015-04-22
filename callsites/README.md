# Overview

This plugin prints a set of tracebacks for a given sink. Each
traceback is represented by an s-expression of the following
syntax:
```scheme
(sink (pred1[ctx1 ctx2 ...] pred2[ctx1 ctx2 ...] ...))
```
where `sink` is the name of the sink, `pred*` is a name of a
caller, and `ctx*` is the context under which the call was performed,
i.e., a set of calls to other functions, that occurs before the call.


# Compilation
```sh
bapbuild -pkgs cmdliner,re.posix callsites.plugin
```
