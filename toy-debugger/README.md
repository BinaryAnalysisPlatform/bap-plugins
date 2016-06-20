## What

This is a toy debugger for various IR interpreter extensions. Right now it
extends concretizer/conqueror. The basic functionality is dumping the CFG and
memory/register values as csv viles. A simple web wrapper let's you navigate
these.

## Using

Have `dot` installed (Needed for svg conversion).

`./run.sh`<br>
`cd viewer && firefox index.html` or whatever browser you prefer.

The viewer is rudimentary. Use arrow keys (left, right) to navigate forward and
backwards.

## Commandline

See the reference in `run.sh`:

```
bap tests/$binary --symbolizer=ida \                                                
  --callsites \                                                                     
  -lbirasm --birasm \                                                               
  -ldebugger --debugger \                                                           
  --debugger-fname=main \                                                           
  --debugger-memory="enter_term" \                                                  
  --debugger-regs="enter_term" \                                                    
  --debugger-dot="enter_term" \                                                     
  --debugger-path-count="enter_term" \                                              
  --debugger-myself="enter_term" \                                                  
  --debugger-checkpoints="enter_term" \                                             
  --debugger-dir="viewer"  
```

The grammar is

```
let grammar () = {|
    flag    ::=  <directives> <hooks>
    directives ::= <directive_1> .. <directive_n>
    directive ::=  <memory>
                 | <regs>
                 | <path-counts>
                 | <trace>
                 | <dot>
                 | <myself>
                 | <checkpoints>
    hooks ::= <hook_1> .. <hook_n>
    hook ::=  <eval_sub>
            | <enter_term>
            | <eval_blk>
            | <eval_jmp>
            | <eval_def>
            | <undefined_var>
            | <undefined_addr>
            | <update>
            | <load>
            | <store>
            | <lookup>
            | <path_terminates>
    |}
```

For instance,

To print the trace after a path terminates, use option
`--debugger-trace="path_terminates"`.

To print the memory state after each load and store, use option
`--debugger-memory="(load store)"`.

Full command:
`bap tests/test --symbolizer=ida -ldebugger --debugger --debugger-fname=main --debugger-memory="(load store)" --debugger-dir="viewer"`

## Future

Note this can serve as a reference for hooking into interpreters and
other extensions (such as taint propagation).

The interpeter hooks may also serve as a reference for developing a proper
debugger in future. Basically, the right way to do this is for each hook to
respond to requests according to a debugging protocol a la V8
https://github.com/v8/v8/wiki/Debugging-Protocol.
