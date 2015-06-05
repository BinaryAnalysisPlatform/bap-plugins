#!/bin/sh

echo "Running plugin on $1..."
bap $1 --loader=bap-elf --use-ida idaq --no-byteweight -lmem_printer
