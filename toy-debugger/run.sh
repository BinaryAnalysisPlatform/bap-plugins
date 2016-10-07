#!/bin/bash

trap convert_svg INT

binary=test1

bap tests/$binary \
  --no-byteweight \
  --symbolizer=ida \
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

function convert_svg() {
  echo "Converting..."
  cd viewer && ls *.dot | xargs -L 1 -I % bash -c "dot -Tsvg % > %.svg"
  cd ..
}

convert_svg
