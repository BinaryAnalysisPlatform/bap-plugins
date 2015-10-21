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
host ::= call | exp | host, host | host .. host
cure ::= '' | call | cure \| cure | cure -> cure | (cure)
call ::= exp = name(args) | name(args)
args ::= exp | exp = int | args, args | ''
name ::= iden | addr | term
iden ::= *C identifier*
addr ::= *hex number*
term ::= *term identifier*
exp ::= var | var[int]
var ::= *CPU register* | $int | $SP | $? | $*
int ::= *integer*
```

Each time a term is executed it is checked whether it matches one of
the infection hosts in the specification. If it is, then term is
infected and new target is created. The target name is built from a
term identifier and the name of host, that started the infection.

For example, if host was `malloc()` then possible target name is
`12ff3324:malloc()`.

A target can be cured. If it is not, then it is either still alive,
and can be cured, or it is dead.

Once target is infected it starts to propagate taint. Values, produced
by the infected target are tainted. Any value, that depends on the
tainted value is also tainted.

The target is cured, if at least one tainted value passes through
sanitization routine, specified by a user in a `cure` part of
specification.

If a target is not cured, but tainted values are still live at the
borders of the quarantine (i.e., accessible in the scope), then the
target is considered to be alive, but uncured.

If a target is not cured, and there are no live values with a target
taint, then it considered dead. That means that target is unsanitized,
and can't be sanitized any more.



## Specifying hosts

### Calls

#### Examples
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

An argument may also contain a constraint in the form of `exp =
constant`. If it is specified, then the argument will be concretized
to the specified constant.

#### Example

The host will match any call to `malloc` and the return value will be
concretized to `0`.

```
R0 = 0 = malloc()
```


### Reads

#### Example

```
    SP[0]
    R0
```

The read host matches whenever the load (from memory) or read (from
register) operation is performed.


### Specifying multiple host

Multiple hosts can be specified in one specification, e.g.,
```
{malloc(), calloc()}
```
is the same as
```
{malloc()} {calloc()}
```

### Specifying host pairs

Sometimes, a infection occurs only if two consequent events occurs in
order. For example, `R0=read_str() .. sql_exec(R0)` denotes, that the
host is created if a taint from a value, returned from `read_str`, is
propagated to the input of `sql_exec(R0)`. The cure must occur
in between the two calls. If the taint has never propagated too
`sql_exec(R0)` either because it was sanitized on the path, or because
there is no dependency at all, then the target is cured and
alive. Otherwise, it is dead.





## Specifying cures

### The default cure

If cure is not specified, then the default cure is used.

The default sanitization procedure requires a tainted value to occur
inside the condition expression of a branch. It is assumed that if the
jump condition has dependency on a host, then it is checked by a
program.

Note, however, that since the evaluation is concrete, the existing
static dependency between tainted value and a jump statement may not
be uncovered, if there was chosen a path that doesn't contain this
jump.

### Call cure

A cure maybe another call, possibly to a sanitization procedure. The
call is specified using the same grammar rules as for the host.

If the call contains arguments, then the taint must pass through
this arguments to be sanitized. If no arguments were specified, then
the taint may be in any live value.

#### Choices

More than one call can be specified as a cure, by separating each call
with `|`. For example `santize() | check()` denotes, that any of the
provided functions will cure the target.

### Chains

Sometimes a cure procedure should be more complex, for example, to be
cured a taint should pass through several sanitizers. Each consequent
element of a chain is specified in order, separated by `->` e.g.,
```
sanitize(R0) -> sql_exec(R0)
```


## Expression language

An expression can be either a variable `var`, a pointer, denoted as
`var[offset]`, where `var` is memory address, and `offset` is a value
that is added to it, e.g., `SP[4]` is a pointer to the `SP+4`.

A register name can be either a register name, that is recognized by a
current architecture, or it can be a special variable, starting from
`$`:

- `$?` - return value;
- `$n` - n'th positional arguments;
- `$*` - all positional arguments

Note, however, that this relies on ABI specification and doesn't work
reliably. For example, to substitute `$*` with a correct set of
expressions, we need to know the arity of the function, that is not
known by default.

Examples
========

Here are some motivating examples

```
{R0 = 0 = malloc()}
{malloc(), calloc(), printf()}
{R0 = read_int() => sanitize(R0)}
{$? = read_int() => sanitize($0)}
{R0 = read_int() => sanitize(R0) -> sql_exec(R0)}
{R0 = read_int() => sanitize(R0) | clean(R0) -> sql_exec(R0)}
{R0 = read_int() .. sql_exec(R0) => sanitize(R0)}
{read_str(SP[4]) => sanitize(R0[4])}
```
