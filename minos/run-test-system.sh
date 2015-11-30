rm -rf analysis-test-system-*

bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test1 \
  --minos-check=system \
  --minos-out_dir=analysis-test-system-1 \
  --minos-srcs=configs/n-1-@system/srcs.txt \
  --minos-sinks=configs/n-1-@system/sinks.txt \
  --minos-with_dots
