# Overview

It is observed that a wide range of problems can be modeled, using a
simple set of inference rules if we enrich our language with a
proposition of form `y -> x`, that denotes that variable `y` depends
on variable `x`. This proposition can be solved either with a static
dataflow engine, a concrete or symbolic execution engine, or
by observing a dynamic behavior of real system. As long as a system
can provide an answer for `y -> x` problem, it can be used as a
backend engine for a generic solver procedure.

Definition: a variable `y` depends on variable `x`, denoted as
`y -> x`, if changing the value of variable `x` affects the value
of variable `y`.

Definition: in `y -> x` the `x` is independent (input) variable, and
`y` is a dependent (output) variable.

We observe a system behavior by asking a backend dataflow engine a set
of questions of the form `y -> x`. We describe a model a system with a
set of mutual recursive definitions, where each definition is an
inference rule. If all rules are satisfied, then a system is
recognized by our model, so we can expect a corresponding
behavior. For example, if we described a model named `malloc_is_safe`
that defines a set of rules, that we expect to hold for systems that
are safe from `malloc` problems, then if all this rules holds, then
our system is safe.

Note: not always a model describes a non-malicious behavior. For
example, we can describe a model of a backdoor. And if a system is
recognized by our model, then it contains a backdoor.

An inference rule (or judgement) is a logical form that consists of a
set of premises and a set of conclusions. Premises and conclusions are
logical propositions, that are defined by a `rule` grammar. Other than
already mentioned dependce rule of a form `var -> var`, it has
syntactic rules that matches terms an IR representation of a
program. For example,

```
when c jump x
```

matches (i.e., holds, evaluates to true) for any term of `jmp` kind.
It also binds condition expression to a `c` variable, and jump
destination to `x`. The bounded variables can be used in dependcy
rules as input or output variables. Rules cannot bind the same
variables (since we cannot establish equalities between them).

Other than bounding a varible, a rule can constraint it, to some BIL
expression:
```
p(R0) = malloc(n(R0))
```

Here, variable `p` is constrained to a value stored in register `R0`
after a call to `malloc`, and variable `n` is constrained to value
stored in `R0` before the call. All input variables (variables that
are used as independent variables in some dependency rule), must be
constrained. If an output variable is constrainted, then a rule will
match only if bound expression is syntactically equal to a constraint
(modulo variable versioning).

Bounded variables, can be used in dependency rules, e.g.,

```
p(R0) = malloc()
when c jmp x
----------------- :: xxx
c -> x
```

This rule defines that a condition of some jump expression must depend
on the value returned from `malloc`.

We also allow a user to specify uninterpreted functions in our
rules. This rules maps to a predicate functions, and allows a user to
extend the model with arbitrary facts. For example, `is_red(v)` can be
defined as a function that returns `true` if term has `color` attribute
equal to `red`, e.g.:

```
x := v
is_red(v)
malloc(y(R0))
-------------- :: red_sinks_into_malloc
y -> x
```

See Examples section for more.

# Grammar

```
model      ::= define ident ::= judgements
judgements ::= '' | judgement judgements
judgement  ::= rules over rules
rules      ::= '' | rule | rule newline rules
rule       ::= call | move | jump | pred | term | var -> var
call       ::= arg = ident(args) | [args1] = name(args2)
move       ::= var := arg | v1[v2] := v3
term       ::= term(args)
jump       ::= when v1 jmp v2
pred       ::= ident(var)
args       ::= arg | arg , args | ''
arg,a      ::= var | v1[v2]
var,v      ::= ident | ident(constr)
constr     ::= exp | exp = word.
over       ::= -----------------... :: ident
```

Conclusion `y -> x` implies that:

1. `x` is an independent variable, bound by one or more rule in
   premise.
2. `y` is a variable that must depend on `x`.



# Examples

## Unchecked malloc

A simpliest model, asserting that each malloc is checked can be
specified by a following two rules:

```
define malloc_is_safe ::=

p(r0) = malloc()
--------------------- :: if_jmp_exists
when c goto e


p(r0) = malloc()
when c goto e
--------------------- :: if_jmp_depends
c -> p
```

We can strengthen our model, by a more sophisticated definition, that
require a mallocated pointer to be checked only if it is used, in that
case we need to add one more premise to both rules:

```
define malloc_is_safe ::=

p(r0) = malloc()
term(q)
q -> p
--------------------- :: if_used_and_jmp_exists
when c goto e

p(r0) = malloc()
term(q)
q -> p
when c goto e
--------------------- :: if_used_and_jmp_depends
c -> p
```

Note: here we used `term(q)` rule that matches all terms and binds all
free variables in it to variable `q`.

This rule will require `c -> p` property to hold iff there exists a
load operation with a pointer that depends on a pointer, that was
returned from malloc (i.e., `q -> p` holds).

## Magic problem


```
define magic_door_exists ::=

is_magic(p)
x(r0) = read()
when c goto d
c -> x
------------------- :: when_magic_meets_user_input
c -> p
```


Rule `magic_door_exists_when_magic_meets_user_input` defines a model
of a backdoor. `is_magic(p)` is a predicate, that is true for a value,
that "looks like a magic value" for us. The rule states, that if jump
condition depends on a magical value and, at the same time, depends on
the user input, then a backdoor may exist. (At least a program matches
with our model of a backdoor).

Example of a backdoor:

```c
int r = read(data, BUFSIZ);

if (strncmp(data, "password", 8) == 0) {
    char *cmd[BUFSIZ];
    sprintf(cmd, "sh %s", &data[8]);
    system(cmd);
}
```

## User controls stack

A simplier example, is a model, that checks whether a user can control
values on stack:

```
define user_can_control_stack ::=

x(r0) = read()
(p=SP)[y] := x
---------------------- :: when_SP_depends_on_user_input
y -> x
```


# Three value logic

In general the proposition of form `y -> x` is not a true logic
proposition, as we can have three answers to this question:

- T (true)  - `y` must depend on `x`.
- F (false) - `y` doesn't depend on `x`
- M (maybe) - `y` may depend on `x`, that means, that it is possible
  that `y` depends on `x` (i.e., exists a path that may or may not be
  feasible, that establishes this property).


Under the three value logic, rules has the following truth table:
```
P T T T F F F M M M
C T F M T F M T F M
R T F M T T T T F M
```
