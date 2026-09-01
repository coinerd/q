#lang racket/base
(require racket/file racket/json
         (file "/home/user/src/q-agent/q/scripts/run-tests/hotspot-benchmark.rkt"))
(define raw (file->string "/tmp/hb-e2e/baseline.json"))
(define m (string->jsexpr raw))
(printf "hash? ~a~n" (hash? m))
(define ks (hash-keys m))
(printf "keys(~a): ~a~n" (length ks) ks)
(printf "key types symbol? ~a~n" (andmap symbol? ks))
(printf "ref \"schema\" -> ~a~n" (hash-ref m "schema" 'MISS))
(printf "errors: ~a~n" (hotspot-manifest-errors m))
