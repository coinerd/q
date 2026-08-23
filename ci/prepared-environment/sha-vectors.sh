#!/bin/sh
# SHA-256 conformance check for manifest.rkt digest subcommand.
cd /home/user/src/q-agent/q || exit 99
fail=0
check() {
  n=$1; f=/tmp/vec-$n.bin
  python3 -c "open('$f','wb').write(bytes([0x61+(i%26) for i in range($n)]))" || exit 99
  exp=$(sha256sum "$f" | cut -d' ' -f1)
  got=$(racket ci/prepared-environment/manifest.rkt digest --file "$f")
  if [ "$exp" = "$got" ]; then echo "$n OK"; else echo "$n MISMATCH got=$got exp=$exp"; fail=1; fi
}
check 0
check 1
check 3
check 4
check 19
check 55
check 56
check 57
check 63
check 64
check 65
check 128
check 1000
exit $fail
