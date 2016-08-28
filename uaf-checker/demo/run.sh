#!/bin/bash

echo "Warning: This will not work as intended without applying microx.patch to your BAP"

mkdir -p viewer
bap --api-add c:posix.h
bap ./gnome-nettool --no-byteweight --symbolizer=ida -l$(pwd)/../main --main --main-precision=2 --main-fname=info_get_nic_information

# Use ida to visualize !
# idaq -S$(pwd)/test-info_get_nic_information.py ./gnome-nettool
