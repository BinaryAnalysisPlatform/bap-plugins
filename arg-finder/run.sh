#!/bin/sh

echo "Running plugin on $1..."
bap $1 --loader=bap-elf --use-ida idaq --no-byteweight -larg_finder --arg_finder-outfile=out.txt  --arg_finder-verbose
