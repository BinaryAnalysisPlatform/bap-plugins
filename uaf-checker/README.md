# Build

* `make`

Optional:
* `./gen_test`
* `make test`

If things happen to fail in the test, things should still be ok. Running
`gen_test.sh` and then rerunning the tests may fix things too.

An example command-line for running is:

`bap tests/all/gnome-nettool --no-byteweight --symbolizer=ida -lmain --main
--main-precision=2 --main-fname=info_get_nic_information`

## Warning:

The demo and tests will not function as expected unless you apply `microx.patch`
to your bap installation. This issue will be resolved once the microx code is
rewritten to support the required option (all paths exploration).

This plugin was built against commit
[33e9e2ec66496e06dc1d0a280d21c86c43b75c6f](https://github.com/BinaryAnalysisPlatform/bap/commit/33e9e2ec66496e06dc1d0a280d21c86c43b75c6f)

# Demo

* `cd demo`
* Just `./run.sh` should work.

Use IDA text search with the following regex and "Find all occurrences" to see
the areas tagged by alloc,use,free:

`~free|~use|~alloc`

E.g.,

<img src=pngs/ida-uaf.png></img>

This demo demonstrates the ability to find the use-after-free in
[gnome-nettool](https://bugzilla.gnome.org/show_bug.cgi?id=753184).

# Description

This plugin performs bounded concrete evaluation with BAP's micro execution
implementation to find use-after-free vulnerabilities. Calls to malloc or free
are annotated with argument and return values according to the C header
specification in `posix.h`

All calls with annotated values are matched according to the regex:

```ocaml
let alloc_result_pat = [".*alloc.*result.*"]
let alloc_size_pat =  [".*alloc.*size.*"]
let free_pat =   [".*free.*ptr.*"]
```

During a given execution, we check each BAP IR `def` statement at evaluation
time. If it matches the above alloc result pattern, we create a pointer of the
form `0xDEADxxxx` with the `alloc_size` range and store this in a map.
`alloc_size` is either a concrete value, or a symbolic value which is
concretized to a random value (see Known Issues below). Correspondingly, if we
see a free pattern, we update the map to reflect that the pointer has been
free'd.

On every load operation, we check whether the address being used to perform the
load has been free'd. When this happens, we output a UAF error. The bookkeeping
of pointers can be expressed as an FSM, but this is implicit in the
implementation.

There are two modes of precision, `1` and `2`. E.g. specified with, (`--main-precision=2`). Precision `1` means: alert only a potential UAF if we observe a use of a base address which we allocated, without caring about the size of the allocation. Precision `2` alerts a potential UAF which falls within the range of a free'd allocation. Here's the code:

```ocaml
let detect used_addr alloced_addr options =
  let open Alloc_data in
  match options.precision with
  | 1 -> Addr.(used_addr = alloced_addr.min)
  | _ -> Addr.(alloced_addr.min <= used_addr && used_addr <= alloced_addr.max)
```

The plugin outputs an ida script which highlights the allocation site, free
site, and use site for each possible detection. It is called
`test-<name_of_function>.py`. See the demo folder for a complete example.

# Known Issues

Here are known limitations to the approach and implementation. In general, false
positives for static analysis is a concern.

* If the allocation size is unknown, (e.g., it would normally determined at
  runtime), the symbolic value is concretized to a random 32-bit value. A better idea
  would be to use a fixed sized memory chunk this case, with smaller sizes being
  more conservative. It's a small change.

* It is particularly difficult to detect UAFs which cross function calls into
  shared libraries. In real-world examples, it is often the case that UAFs occur
  in such cases. This plugin does not yet support such reasoning. Moreover, BAP
  doesn't currently support the inclusion of shared library functions within the
  same project (although this is conceivable).
