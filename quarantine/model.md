# Overview

It is observed that a wide range of problems can be efficiently
modeled, as a search problem over a program structure, if we enrich
the problem description with a simple constraint language, that
contains only one proposition of form `y / x`, that denotes that
variable `y` depends on variable `x`.

Definition: a variable `y` depends on variable `x`, denoted as
`y / x`, if changing the value of variable `x` affects the value
of variable `y`.

## Searching procedure

Our search procedure will operate on the Intermediate Representation
(IR) of a program, and will search for particular substructures of the
program. The input of the search procedure is a pattern and a set of
constraints. The search procedure will return a sequence of
substructures, that matches the given pattern. Consider the following
example:

```
 var {x,y} s.t. {x=R0, y=SP}
 x := y
```

where `var {x,y} s.t. {x=R0, y=SP}` is read "declare variables `x` and
`y`, such that `x` is syntactically equal to `R0` and `y` is equal to
`SP`.

Note: here `x` and `y` are variables of our domain specific language,
where `R0` and `SP` are concrete variables of the BIL language. We can
say that `x` is a metavariable, that ranges over all BIL variables
(although in the example above the range is constrained to a singleton
set).

Pattern `x := y` matches any definition (def term), and binds `x` to
BIL variable that is defined (the left hand side), and `y` to any free
variable that occurs on the right hand side of a definition.

In the following program terms `4` and `6` will match the pattern:

```
1: program example1
2: sub f1:
3: R1 := SP
4: R0 := SP + 4
5: R0 := R1 + R0
6: R0 := mem[SP+R0]
```

Term `5` doesn't match, because in the right hand side of the def term
doesn't contain `SP` as a free variable, so there is no free variables
in it that satisfies constraint `y=SP`.


The `x := y` term is one example of a pattern, that are actually atoms
of our language. Three more patterns are provided:
- call - matches a call to subroutine;
- jump - matches a transfer of control;
- wild - matches any term.


The `y/x` relation denotes a partial order over terms, that allows us to
introduce sequencing operator. Consider the following example:

```
 var {x,y} s.t. {x=R0, y=SP, q/x}
 x := y, p := q
```

It matches two definition, such that the second definition, uses some
variable, that depends variable `x` defined in first definition. In
program `example1`, a search procedure will find a sequence of two
terms, that matches this pattern, namely `4,6`.

Since `x/y` relation is transitive, we can match sequences of
arbitrary length. For example, the following expression will match
three terms, that occurs in a sequence (since they have data
dependency between them).


```
var {p,q,r,s,x,y} s.t. {q/y,s/q}
x := y, p := q, r := s
```

In program `example1` it will match to the following sequences of
terms: `{3,5,6}` and `{4,5,6}`.



## Decision procedure

Armed with an ability to search for a given pattern in a program, we
can describes models of rather sophisticated programs. We will
describe a model of a program with a set of inference rules, of the
form `p1,..,pN |- q1,q2,..,qN`, where `p` and `q` are patterns,
describe above, denoting premises and conclusions correspondingly.

If in a program exists such sequence of terms, that matches premises,
and all conclusion, then we say, that this sequence is recognized by
our model and all rules are satisfied. And thus the sequence manifest
a behavior that we have associated with that model. This sequence of
terms also acts as a constructive proof.

But, if there exists, such sequence of terms, that matches only the
premises, but there exists at least one conclusion, that is left
unmatched, then we say, that that we found a particular sequence of
terms, that violates our rules, and this sequence acts as a
constructive proof, of the fact that the logical implication doesn't
hold.

Basically, we define our model as set of assertions, and check that
this assertions hold for all combinations of terms.

As a result, we categorize all substructures of a program structures into
three categorizes:

- unrecognized;
- satisfied;
- unsatisfied.

We drop off the unrecognized set, and inspect either satisfied or
unsatisfied sets, based on concrete set of rules. (Our problem
descriptions can model both desirable, or undesirable behavior,
depending on what is easier to work with).

Let's go to some concrete examples:


```
define malloc_is_safe ::=
  var {p,c,e} s.t. {c/p, p = R0}
  rule if_some_jmp_depends ::=
    p := malloc() |- when c = jmp e
```

This model will recognize all calls to `malloc` function, and classify
them into two categories:

- calls that are checked;
- calls that are unchecked.

It is obvious, that calls that are checked may still be unsafe (so the
name of the model is too optimitic). In this examples, we're more
interested in calls that are not checked. With this rule, the decision
procedure will mark all calls to `malloc` that are not followed by
check as non-satisfying rule `malloc_is_safe_if_some_jmp_depends`.

From a theorem prooving perspective, the descision procedure, given a
set of facts, of the form:

```
-----, -------, ...
x / z   x = R0
```

provided by a dataflow engine and search procedure, will try to find
such deriviation, that proofs rules of specified system.


# Grammar


```
model      ::= define ident ::= decls rules
decls      ::= decl1 .. declN
rules      ::= rule1 .. ruleN
decl       ::= var {v1,..,vN} s.t. {c1,..,cM}
rule       ::= rule ident ::= pat1,..,patN |- p1,p2,..,pM
pat,p      ::= call | move | jump | wild
cons,c     ::= v1 / v2 | v = id | v = word | pred
call       ::= e1,..,en := id(e1,..,en) | id(e1,..,en)
move       ::= v := exp | *v1 := v2
jump       ::= when v1 jmp_kind v2
wild       ::= v
pred       ::= id(v)
jmp_kind   ::= "call" | goto | ret | exn | jmp
exp,e      ::= var | *var
id,v       ::= ?string identifier?
ident,id   ::= ?string identifier?
```


## Notes


We describe our problem with a set of logical formlae of a form
`premises |- conclusions` and a set of constraints:


```
define problem_name ::=
  var {v1,..,vn} s.t. {c1,..,cm}
  rule rule1 ::= p1,..,pN |- s1,s2,..,sM
  ..
  rule ruleL ::= q1,..,qi |- r1,r2,..,rk
```


Where,

```
var {v1,..,vn} s.t. {c1,..,cm}
```

describes a set of constraints, with `v` and `c` used as
metavariables, standing for variable and constraint corresponding.

For example:

```
var {p,q} s.t. {q/p}
```

declares variables `p` and `q` such that `q` depends on `p`.

Each logical formula, called `rule` in our parlance, denotes an
assertion, that must hold in our model of a program. It has a form of
inference rule written in a concise form: `premises |- conclusions`.

Premises and conclusions, are described as patterns, that structurally
match terms of a program in Intermediate Representation (IR).

Each pattern must bind different variables.

We also allow a user to specify an uninterpreted function as a
constraint. They maps to a user provided functions allows a user to
extend the model with arbitrary facts. For example, `is_red(v)` can be
defined as a function that returns `true` if term has `color`
attribute equal to `red`, e.g.:

```
var {x,y,v} s.t. {y/x, y=R0, is_red(v)}
rule red_sinks_into_malloc ::=
  x := v |- malloc(y)
```

# Examples

## Unchecked malloc

A simpliest model, asserting that each malloc is checked can be
specified by a following definition:

```
define malloc_is_safe ::=
  var {p,c,e} s.t. {c/p, p = R0};
  rule if_some_jmp_depends ::=
    p := malloc() |- when c jmp e
```

We can strengthen our model, by a more sophisticated definition, that
require a mallocated pointer to be checked only if it is used, in that
case we need to extend one premises with one more rule:

```
define malloc_is_safe ::=
  var {p,c,e,t} s.t. {c/p, p = R0}
  rule if_used_and_some_jmp_depends ::=
    p := malloc(), t |- when c jmp e
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
before.


## Magic problem

```
define magic_door_exists ::=
  var {d,x,p,v} s.t. {is_magic(v), x=R0, c/x, c/p}
  rule when_magic_meets_user_input ::=
    p := v, x := read() |- when c jmp d
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
  var {x,y,p,s,z} s.t. {x=R0, s=SP, p/x, p/s}
  rule when_SP_depends_on_user_input ::=
    x := read, s := y |- p := z
```

```
define cmdline_can_control_stack ::=
  var {x,s,y,p,z} s.t. {s=SP, in_main(s), r/s, p/x}
  rule when_SP_depends_on_main_arg ::=
    s := y, x := *r |- *p := z
```

```
define stack_is_guarded ::=
  var {c,d,p,s,x,y} s.t. {s=SP, p/s, c/p}
  rule when_sp_is_checked_before_write ::=
    s := y, *p := x |- when c jmp d
```

## SQL sanitization

We may assert that data obtained from the outside world will never
reach `sql_exec` function, without being sanitized with `sql_escape`
function, with the following definition:

```
define sql_exec_is_safe ::=
  var {p,x,y,z} s.t. {x=R0, p/x, y/x, p/z}
  rule if_user_input_is_sanitized ::=
    x := read(), sql_exec(p) |- z := sql_escape(y)
```

We can actually, make it even stonger, by stating that any pointer
should be passed through sanitization procedure, before execution.

```
define sql_exec_is_safe ::=
  var {x,p,u,x,y,z} s.t. {p=R0, p/x, y/x, p/z}
  rule if_input_is_sanitized ::=
  x := u, sql_exec(p) |- z := sql_escape(y)
```

## Unchecked user input

User input, that may pass to sensible code paths, e.g., to malloc
input, load or store operations, must be checked and, ideally, checked
and then redefined. For example, the following code:

```c
char *img;
int rows, cols, size, has_color;
fscanf(fp, "%d %d %d", &rows, &cols, &size);
has_color = size != (rows * cols);
img = malloc(size);
```

the `size` variable is checked, but its result is not redefined,
so the check cannot sanitize the user input

```
define user_input_is_sanitized ::=
  var  {x,y,u,u',v,v'}
  s.t. {user_input(u), critical(v), u'/x, v/x, v'/y}
  rule if_checked  ::= x := u, v |- when u' jmp _
  rule if_assigned ::= y := u, v |- x := v'
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

# Requirements for dataflow engines

The proposition `y / x` can be solved either with a static dataflow
engine, a concrete or symbolic execution engine, or by observing a
dynamic behavior of real system. As long as a system can provide an
answer for `y / x` problem, it can be used as a backend engine for a
generic solver procedure.

## Proposed API

The solving procedure is split into three parts:
- seeding;
- raising;
- reaping.

In seeding phase the solver analyses rules and marks definitions, that
defines `x` for all `y/x` predicates with `Taint.seed` attribute.

In raising phase a dataflow engine must propagate taints introduced by
seeded terms throught the program. Each term, which is reached by the
taint, must be annotated with a `Taint.vars` attribute, that maps each
free variable in the term to a set of taints, annotated with `must` or
`may` tag.

In reaping phase the solver uses the data dependency information,
provided by a dataflow engine, to proof or disproof all rules.
