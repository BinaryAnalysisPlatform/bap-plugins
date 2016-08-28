#!/bin/bash


bap --api-add=c:posix.h

expected=expected
mkdir -p $expected
for b in `ls --ignore=\*.c --ignore=Makefile --ignore=\*.h --ignore=\*.so --ignore=\*.o tests/all`; do
    bap tests/all/${b} --no-byteweight --symbolizer=ida -lmain --main --main-fname=main &> $expected/${b}.output
done

# overwrite gnome-nettool
bap tests/all/gnome-nettool --no-byteweight --symbolizer=ida -lmain --main --main-precision=2 --main-fname=info_get_nic_information --callsites &> $expected/gnome-nettool.output
