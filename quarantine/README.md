Overview
========

This plugin allows to set a quarantine zone around a particular
program point, and infect some variable, in order to observe, how the
infection will spread through this zone, through concrete evaluation.

Currently, the starting point, should be a name, address or tid of a
function. Then infection point, should be some def term. It is
possible to infect more than one term at once.

The infection is propagated from one value to another, if value result
depends on infected value. For example, if `R0 := R1 + 1`, and `R1` is
infected, then `R0` will become infected also, and will spread the
infection furthermore. See, [taint.mli] for more formal rules on how
infection is propagated.

Each infection is designated by its point of origin, i.e., by the tid
of the definition, that was originally infected by a user. After
plugin pass has finished, each variable in each term is associated
with a set of taints, that infected it.

A point is infected by attaching a `Taint.seed` attribute to some
`def` `term`. Results, can be queried, by looking at `Taint.vars`
attribute of any term.
