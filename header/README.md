# Overview

This plugin will fill in arguments in subroutines. It takes a C file
and parses it using FrontC library to get function prototypes. It infers
arg terms from C declaration, it also tries to infer argument expressions
based on his (very limited) knowledge of ABI.

# Building

The plugin requires `FrontC` library, so make sure that you have it
installed. For example, with opam:
```
$ opam install FrontC
```

After this, just issue `make` command to get the plugin file.

# Usage

You can use the `stdlib.h` file, that enumerates about 250 functions
from C standard libary. You can write your own file. The typical usage
pattern will look like this:

```
$ bap binary -lheader --header-file=libc.h -dbir
```

You can even try to feed in a system header file, but FrontC is
usually not capable of parsing them (neither do I), so it is easier
to write prototypes by hand. Note however, that FrontC will fail, if
it meets unknown type, so make sure, that you also add declarations.


# Example

Without `header`:

```
00000017: sub malloc()
00000011:
00000012: R12 := 0x8308:32
00000013: R12 := R12 + 0x8000:32
00000014: base_239 := R12
00000015: R12 := R12 + 0xD04:32
00000016: goto mem[base_239 + 0xD04:32, el]:u32

0000002c: sub exit()
00000026:
00000027: R12 := 0x832C:32
00000028: R12 := R12 + 0x8000:32
00000029: base_260 := R12
0000002a: R12 := R12 + 0xCEC:32
0000002b: goto mem[base_260 + 0xCEC:32, el]:u32


00000033: sub abort()
0000002d:
0000002e: R12 := 0x8338:32
0000002f: R12 := R12 + 0x8000:32
00000030: base_267 := R12
00000031: R12 := R12 + 0xCE4:32
00000032: goto mem[base_267 + 0xCE4:32, el]:u32
```

With `header`:
```
00000017: sub malloc(size, res)
000001ed: size :: in u32 = R0
000001ee: res :: out u32 = R0
00000011:
00000012: R12 := 0x8308:32
00000013: R12 := R12 + 0x8000:32
00000014: base_239 := R12
00000015: R12 := R12 + 0xD04:32
00000016: goto mem[base_239 + 0xD04:32, el]:u32

0000002c: sub exit(code)
000001ef: code :: in u32 = R0
00000026:
00000027: R12 := 0x832C:32
00000028: R12 := R12 + 0x8000:32
00000029: base_260 := R12
0000002a: R12 := R12 + 0xCEC:32
0000002b: goto mem[base_260 + 0xCEC:32, el]:u32


00000033: sub abort(x1)
000001f0: x1 :: in u32 = R0
0000002d:
0000002e: R12 := 0x8338:32
0000002f: R12 := R12 + 0x8000:32
00000030: base_267 := R12
00000031: R12 := R12 + 0xCE4:32
00000032: goto mem[base_267 + 0xCE4:32, el]:u32
```


# Problems


## State of ABI support

So far `arm-linux-gnueabi` is more or less supported.  The System V
amd64 abi, is much worse, especially when we're dealing with types of
class memory (in general, we need to know size of passed object, that
is out of scope of this plugin). The `cdecl` abi is fairly easy, and
looks like that it is supported, although I didn't check it very
close.

Also, there is no choice of ABI - for each target, there is only one
ABI. This is limitation is due to a simplicity of taken approach.
