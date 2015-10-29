# Overview

It is observed that a wide range of problems can be modeled, using a
simple set of inference rules if we enrich our language with a
proposition of form `y / x`, that denotes that variable `y` depends
on variable `x`. This proposition can be solved either with a static
dataflow engine, a concrete or symbolic execution engine, or
by observing a dynamic behavior of real system. As long as a system
can provide an answer for `y / x` problem, it can be used as a
backend engine for a generic solver procedure.

Definition: a variable `y` depends on variable `x`, denoted as
`y / x`, if changing the value of variable `x` affects the value
of variable `y`.

Definition: in `y / x` the `x` is independent (input) variable, and
`y` is a dependent (output) variable.

We observe a system behavior by asking a backend dataflow engine a set
of questions of the form `y / x`. We define a model of a system
with a set of inference rules. If all rules are satisfied, then a system is
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
logical propositions, that are defined by a `rule` grammar. Each
proposition consists of pattern and a set of constraints. Pattern will
match with a term, if all constraints are satisfied, e.g,

```
v := x, v = SP
```

matches (i.e., holds, evaluates to true) for any load operation, from
SP based address.

A scope of a variables, bounded by a pattern, is the whole judgement,
so it may be used in other rules of the same judgement:

```
p := malloc(n), p = R0, n = R0
when c jmp x, p / c
```

Each pattern must bind different variables.

Each judgement consists of a set of premises. If all premises hold,
then all conclusions must hold also. If it doesn't then system doesn't
match the model.

```
p := malloc(), p = R0
--------------------- :: when_checked
when c jmp x, p / c
```

This rule defines that a condition of some jump expression must depend
on the value returned from `malloc`.

We also allow a user to specify an uninterpreted function as a
constraint. They maps to a user provided functions allows a user to
extend the model with arbitrary facts. For example, `is_red` can be
defined as a function that returns `true` if term has `color`
attribute equal to `red`, e.g.:

```
x := v, is_red
-------------------- :: red_sinks_into_malloc
malloc(y), y/x, y=R0
```

See Examples section for more.

# Grammar

```
model      ::= define ident ::= judgement,.., judgement
judgement  ::= rules over rules
rules      ::= rule \n..\n rule
rule       ::= patt , cons
patt       ::= call | move | jump | wild
cons       ::= v1 / v2 | v = id | v = word | v1,..,vn
call       ::= e1,..,en := id(e1,..,en) | id(e1,..,en)
move       ::= v := exp | *v1 := v2
jump       ::= when v1 jmp_kind v2
wild       ::= var
pred       ::= id | id(v1, v2,.., vn)
jmp_kind   ::= call, goto, ret, exn, jmp
exp,e      ::= var | *var
var,v      ::= id
ident,id   ::= ?string identifier?
over       ::= --..-- :: ident
```

# Examples

## Unchecked malloc

A simpliest model, asserting that each malloc is checked can be
specified by a following definition:

```
define malloc_is_safe ::=

p := malloc(), p = R0
--------------------- :: if_some_jmp_depends
when c jmp e, c / p
```

We can strengthen our model, by a more sophisticated definition, that
require a mallocated pointer to be checked only if it is used, in that
case we need to extend one premises with one more rule:

```
define malloc_is_safe ::=

p := malloc(), p = R0
t, t / p
--------------------- :: if_used_and_some_jmp_depends
when c jmp e, c / p
```

This rule will require `c / p` property to hold iff there exists a
load operation with a pointer that depends on a pointer, that was
returned from malloc (i.e., `q / p` holds).

Note: here we used a wildcard `t` rule that matches all terms and
binds all free variables in it to variable `t`. Basically, `t, t/p`
can be read as "there exists such term that has a free variable,
depending on p"

However, this model still allows programs, that are unsafe, in
particular, it allows programs, that checks pointer after use, not
before. We may try to constraint our system furthermore,

```
define malloc_is_safe ::= # not a valid formula

p := malloc(), p = R0
*r := x, r / p
--------------------- :: if_used_and_some_jmp_depends
when c jmp e, r / c
```

The rule, looks correct, as it constains the system, so that the use
must always depend on the jump. But this rule is malformed, since
constraint `r/c` require `c` variable to be defined somewhere, i.e.,
there should exist a patter, where `c` occurs to the left of `:=`
symbol. But here `c` is bound inside the jump condition. Since, `c` is
never defined, we cannot observe it.

So such precision is not supported by our formalism.

## Magic problem


```
define magic_door_exists ::=

p := v, is_magic(v)
x := read(), x = R0
---------------------------- :: when_magic_meets_user_input
when c jmp d, c / x, c / p
```


Rule `magic_door_exists_when_magic_meets_user_input` defines a model
of a backdoor. `is_magic(p)` is a predicate, that is true for a value,
that "looks like a magic value" for us. The rule states, that if jump
condition depends on a magical value and, at the same time, depends on
the user input, then a backdoor may exist. (At least a program matches
with our model of a backdoor).

Example of a backdoor, that matches this model:

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

x = read(), x = R0
s = y, s = SP
--------------------- :: when_SP_depends_on_user_input
*p := z, p / x, p / s
```

```
define cmdline_can_control_stack ::=

s = y, s = SP, in_main
x = *r, r / s, in_main
--------------------- :: when_SP_depends_on_main_argument
*p := z, p / x
```

```
define stack_is_guarded ::=

s = y, s = SP
*p = x, p / s
-------------------- :: when_sp_is_checked_before_write
when c jmp to, c / p
```

## SQL sanitization

We may assert that data obtained from the outside world will never
reach `sql_exec` function, without being sanitized with `sql_escape`
function, with the following definition:


```
define sql_exec_is_safe ::=

x := read(), x = R0
sql_exec(p), p/x, p = R0
------------------------------ :: if_user_input_is_sanitized
z := sql_escape(y), y/x, p/z
```

We can actually, make it even stonger, by stating that any pointer
should be passed through sanitization procedure, before execution.

```
define sql_exec_is_safe ::=

x := u
sql_exec(p), p/x, p = R0
------------------------------ :: if_input_is_sanitized
z := sql_escape(y), y/x, p/z
```



# Three value logic

In general the proposition of form `y / x` is not a true logic
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
