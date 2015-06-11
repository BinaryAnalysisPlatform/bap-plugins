# CFG Printer

This is a very simple plugin mostly for demonstrating purposes. It
shows how to use command line, register plugins and print CFG.

## Compilation

Go to the folder, where `print_cfg.ml` resides and do the following:
```sh
$ bapbuild print_cfg.plugin

```

## Usage

To load plugin first compile it, then load it with:

```sh
$ bap -lprint_cfg /bin/ls
```

It will bark on you, saying that you didn't specify a required
argument. To get the information about command-line arguments,
use the following command:

```sh
$ bap -lprint_cfg /bin/ls --print-cfg-help
```

It will tell you, that this plugin has one required argument, called
`--symbol`. Recalling, that parameter with name `--xxx` is passed to a
plugin with name `ppp` as `--ppp-xxx` we can invoke it. But you need
to figure out a name of a function. If you know, then just use it. If
you trying just to print some function, then you can query bap for
function names with `--print=name` option (or `-pname`) for short.

```sh
$ bap /bin/ls -pname
```

Most likely, that `/bin/ls` in your system is compiled without a
debugging information, and names will be meaningless. But
nevertheless, we still can try (with arbitrary name, that with high
probability will not work on your system):

```sh
$ bap -lprint_cfg /bin/ls --print-cfg-symbol=sub_40eb16
```

If you're playing with your own binary, make sure, that you're
compiling with `-g` option.

Now, you can view the CFG with your favorite viewer, e.g.,

```sh
$ xdot sub_40eb16.dot
```
