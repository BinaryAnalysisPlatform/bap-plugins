Quarantine is a taint propagation framework. It uses microexecution to
propagate the taint through a program. The execution is perfomed using
the ConquEror Engine, that is short for Concrete Evaluation with
Errors. This execution engine allows to run incomplete programs with
an unspecified user input. Moreover, to increase the coverage it may
take infeasible paths.

The taint is propagated from a seed to its maximum extent. The seed is
a definition point that is marked with a `Taint.reg` or `Taint.ptr`
tag. A usual way of using the framework, would be to use one or more
passes that marks points of interest with a taint seed, then to use
the quarantine pass to propagate the taint, and, finally, to use a
pass that will collect and analyze the result. The quarantine itself
doesn't provide any analysis, other than the ability to highlight
terms with specific colors.

The microexecution is performed over a lifted program using Biri
interpreter. Memory reads are intercepted and redirected to program
image, if possible (for static data), otherwise they are concretized.
All other inputs, like reads from unknown registers or user input are
also concretized. Several concretization policies are provided:


      - Const - all unknown values are concretized to a specified constant;
      - Random - a random value is picked from a value domain;
      - Range - a random value is picked from a specified range.

By default, the microexecution engine tries to visit all program
branches. During the execution, it will record missed branches as
checkpoints. When there is nothing more to explore, it will backtrack
to a stored checkpoint, restoring the execution state at this program
point, and continue the execution. Of course, in this case the state
will contradict with a path constraint. In a deterministic mode the
bactracking mechanism is disabled. In this mode, no checkpoints are
recorded, and whenever the interpreter requests a backtracking, it
will instead return from a current procedure.

The maximum length of an execution path is limited with some constant
number of jumps (basic blocks). Also, a loop escaping mechanism, will
detect loops and bail out of them after a specified amount of
iterations. In the deterministic mode it will just return from a
procedure, otherwise, it will backtrack.
