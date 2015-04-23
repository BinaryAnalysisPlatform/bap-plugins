# Overview

This plugin reads from standard input (later I will parametrize the
input with command line option) specification of routes and checks,
that whether it is satisfied or not. The route is specified with the
following grammar:

```
  route = point, sep, {point}, sep, point, eol;
  point = hex | str;
  sep = " " | ";" | "," | " ";
  eol = "\r" | "\n" | "\n\r" | "\r\n";
  hex = ?conventional hexadecimal number?;
  str = ?anything that doesn't starts with 0?;
```

Less formally, route is a sequence of points, where the first point is
the source of the route, last is the destination point and whatever in
between is checkpoints. Point can be and address, or a string denoting
the symbol name.

The route is valid if it exists, i.e., there is a path from source
point to the destination point (which implies that both points should
also exist), and for each checkpoint there exists a path that passes
through it. This can be different paths, i.e., it is not guaranteed that
there exists a path that visits all specified checkpoints.

# Usage

## Interactive

For interactive use it is suggested to use `rlwrap` or similiar
utility,

```sh
rlwrap bap exe -lcheckpaths
```

Here is the example of interaction:

```sh
$ rlwrap bap paths -lcheckpath
> a c b
> a f b
[FAIL]: main@0x40052F:64 violates a -> f -> b
> a happens b
[FAIL]: no such checkpoints: happens
>
```

No output means, that path is valid. If path is invalid, it will be
printed. Possible outputs:
```
1. [FAIL]: <callsite> violates <src> -> <missed-check-point> -> <dst>
2. [FAIL]: no such checkpoints: <point>, [<point>]
```
The second variant is printed, when requested checkpoint wasn't find
in the file at all, but paths may exists. Although, it is not
guaranteed the path from [src] to [dst] do exist. So this should be fixed.

## Batch mode

Just redirect a file with route specifications to `bap`, e.g.,

```sh
$ bap paths -lcheckpath < routes
```


# Compatibilty

Requires BAP 0.9.6 with #191 changeset applied.
