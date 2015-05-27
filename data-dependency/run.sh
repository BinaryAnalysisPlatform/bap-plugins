#!/bin/bash

bap $1 -ldata_deps --data_deps-infile=in.txt -ltoida --data_deps-idascript=script.py #--emit-ida-script=script.py
idaq64 -OIDAPython:`pwd`/script.py $1
