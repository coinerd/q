#!/bin/sh
# Local sanity check: manifest.rkt digest matches coreutils sha256sum.
set -u
cd "$(dirname "$0")/../.." || exit 1
ok=1
for f in /tmp/abc.txt /tmp/empty.txt /tmp/fox.txt ci/racket-package-lock.rktd; do
  [ -f "$f" ] || { echo "missing $f"; ok=0; continue; }
  m=$(racket ci/prepared-environment/manifest.rkt digest --file "$f") || ok=0
  s=$(sha256sum "$f" | cut -c1-64)
  if [ "$m" = "$s" ]; then
    echo "MATCH  $f $m"
  else
    echo "DIFFER $f manifest=$m sha256sum=$s"
    ok=0
  fi
done
[ "$ok" = 1 ] && echo "ALL DIGESTS MATCH" || echo "DIGEST MISMATCH"
exit $((1 - ok))
