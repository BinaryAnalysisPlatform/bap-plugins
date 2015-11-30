# Minos

Minos generates all static paths between two program points in a program.
Currently, these program points are callsites of a source and sink function,
and will later be extended to handle any program point (tid in BAP).
A source block, for example, could be a block that calls a specific function,
such as a `read()` input function. Alternatively, the source block could also
be set to be the entry block of a function.

Minos achieves static path enumeration by first processing an entire program
graph into subgraphs with respect to source and sink, and further transforms
these subgraphs into DAGs by removing back edges. When paths between source
and sink cross procedure boundaries, inlining is performed (for non-recursive
procedures).

## Static checks

A suite of static checks currently exist for Minos. Each check parameterizes
Minos in the following ways:

1. The conditions under which all static paths should be enumerated are encoded in
the `should_produce` function.

2. The checks that should be performed are encoded in the `check_path` function.

3. Direction: whether paths should be enumerated from a sink to a source, or
   vice versa

4. The maximum number of blocks that should be considered for a path (`max_depth`)

5. The number of paths to `sample`, corresponding to a subset of the total number of paths (for cases in which too many paths are produced)

6. A per-path `timeout` parameter if a path cannot be checked in a specified amount of time

The `should_produce` predicate usually guards against generating paths that
are either uninteresting (because arguments to functions are constants) or
because a particular trim produces too many paths in the DAG. Alternative ways
to curb analyses are available through the `max_depth`, `sample`, and `timeout` options.
See `check.mli` for the corresponding type definitions.

#### A sample check : system

Associated script: `run-test-system.sh`. The sink is `system`, the source is
the entry point point of the calling procedure.

1. Path generation conditions: Input to `system()` is not a constant (string).

2. Path check predicates:

a) Input is symbolic
b) sprintf or snprintf occurs before the call
c) there is no predicate that the argument is data dependent on
d) the argument dependence does not extend beyond this block

Read the following as "Priority 1 is output if a,b,c,d is satisfied"

* 1 -> a,b,c,d
* 2 -> a,b,c
* 3 -> a,b
* 4 -> a,c
* 0 -> a

3. Path direction: Reverse

4. Default depth of path: 100

5. Default number of paths: 30

6. Sample: no limit

7. Timeout: infinite

#### Included checks

Modules for static checks are still under heavy development, and subject to change.
Currently, the following checks are provided as PoC examples:

* `system_check` : check for system arguments
* `memcpy_check` : check for unchecked length parameter to memcpy
* `strtol_check` : check for unchecked destination pointer (detect possible cases where destination pointer can be NULL)
* `sql_check`    : check for modification of a string without escape, before being executed

## Options

Here are some of the more important plugin options:

* `with_dots` : output dot files of cuts and trims, highlighting the
source and sink blocks. A dot output of a cut will give show you the scope
(context) in which the source and sink block lives, in terms of subroutines. A dot output
of trims will show you the cut with all unreachable nodes between the source and sink
block trimmed out.

* `cuts_only` : produce only the cut groups between a source and sink.

* `trims_only` : produce only trims between a source and sink, after producing cut groups

* `path_counts_only` : print out a path counter as paths are enumerated. Do not process
or analyze the paths

* `output_dot_path` : output a dot of the trim, with the path highlighted. Warning:
this is typically very expensive to run. Useful for debugging.

* `out_dir` : specify an output directory. The default is `./analysis`.

* `srcs` and `sinks` : specify files containing the source and sink of interest

## Usage

Output is found in the analysis directory which is structured as follows
(produced with the `with_dots` option):

```
analysis-test-system-1/
├── cut_groups
│   ├── cuts.txt                                       // summary of cut groups
│   └── valid_cut_0000                                 // instance of cut group
│       └── valid_cut_case_0_%00000162_%00000177.dot   // dot output of cut group
├── meta.txt                                           // overall meta information of this run
├── misc.txt                                           // miscellaneous analysis output
└── trim_groups
    ├── trim_0000_case_0000                            // instance of trim
    │   ├── flagged_0000                               // whether this trim satisifed 'should_produce'
    │   ├── paths                                      // directory with instances of paths produced
    │   │   ├── 0000.path                              // BIR output of path
    │   │   └── priority_0002                          // priority for checking from `check_path`
    │   ├── paths.txt                                  // summary of path information (number of paths)
    │   └── trim_%00000162_%00000177.dot               // dot output of trim
    └── trims.txt                                      // summary of trims
```

## Comments

Minos is suitable for a variety of tasks. For instance, it can answer broad
questions such as:

* How many unique pairs of calls to function X and Y are reachable within a program?

* How many paths exist between reachable pairs of calls to function X and Y in the program? (with respect to a DAG, discounting loops and recursive calls)

It can be used to perform further specific queries such as:

* What is the nature of arguments passed to the `system` call in the program? Are they
  constant strings, or symbolic (stored in a register and known only at runtime)?

* What is the nature of the length argument passed to the `memcpy` call in the program?
Is it a constant value, or symbolic? Is this value checked?

The example queries allude to the fact that Minos currently supports additional
features, such as:

a) Resolving strings in binaries which are referenced by constants
b) Inferring arguments to a number of libc functions
c) Constant propagation and constant folding on paths
d) Determining data dependence of register (i.e. non pointer) arguments to libc
functions
e) Indirect call resolution along paths when possible through constant folding

## Run scripts

`run-test-system.sh` is the go-to example for interacting with Minos.

