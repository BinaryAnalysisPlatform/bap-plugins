Overview
========

Note: this is not implemented yet. Just ideas.

Quarantine will set up a zone around a particular point and classify
all targets in that zone into three categories:

* Cured
* Uncured
* Dead


The classification is done by concretely executing a program at the
specified point and checking policies, that are provided by a user.

A specification (see grammar below), provided by a user, defines a
policy, that describes how targets are infected, and how they can be
cured.

```
spec ::= { host => cure } | {host} | spec spec | ''
host ::= call | exp | host, host
cure ::= '' | call | cure \| cure | cure -> cure
call ::= exp = name(args) | name(args)
args ::= exp | exp, args | ''
exp ::= var | var[off]
var ::= *CPU register*
off ::= *signed integer*
```

Each time a term is executed it is checked whether it matches one of
the infection hosts in the specification. If it is, then term is
infected and new target is created. The target name is built from a
term identifier and the name of host, that started the infection.

For example, if host was `malloc()` then possible target name is
`12ff3324:malloc()`.

A target can be cured. If it is not, then it is either still alive,
and can be cured, or it is dead.


## Hosts

### Calls

Examples:
```
    R0 = malloc()
    malloc()
    printf(R0,SP[4],SP[8])
```


Matches whenever a call to a specified subroutine is performed.  A
call may contain arguments, if it is, then the specified arguments
are tainted. Otherwise a default one argument, defined by an ABI
is tainted. The order and position of arguments doesn't matter.

For example: `malloc()` and `R0 = malloc()` and `malloc(R0)` means
the same in ARM gnueabi.

Multiple hosts can be specified in one specification, e.g.,
```
{malloc(), calloc()}
```
is the same as
```
{malloc()} {calloc()}
```

### Reads

Examples:
```
    SP[0]
    R0
```














Examples
========

```
{malloc(), calloc(), printf()}
{R0 = read_int() => sanitize(R0)}
{R0 = read_int() => sanitize(R0) -> sql_exec(R0)}
{R0 = read_int() => sanitize(R0) | clean(R0) -> sql_exec(R0)}
{read_str(SP[4]) => sanitize(R0[4])}
```
