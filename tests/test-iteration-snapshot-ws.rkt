#lang racket/base

;; BOUNDARY: integration

;; tests/test-iteration-snapshot-ws.rkt -- T-03: TR-boundary test
;;
;; Verify that iteration-snapshot can be constructed from untyped code
;; with a real working-set (or #f) and passes through the TR boundary
;; without any-wrap/c contract failure.

(require rackunit
         (only-in "../runtime/iteration/loop-state.rkt"
                  iteration-snapshot
                  iteration-snapshot?
                  iteration-snapshot-counters
                  iteration-snapshot-ws
                  iteration-snapshot-config
                  iteration-snapshot-max-iterations
                  loop-counters
                  loop-counters?)
         (only-in "../runtime/session-config.rkt" session-config? hash->session-config))

(test-case "T-03: iteration-snapshot with #f ws passes TR boundary"
  ;; Construct a minimal session-config for the config field
  (define cfg (hash->session-config (hasheq)))
  ;; Construct a minimal loop-counters
  (define ctrs (loop-counters 0 0 '() 0 0 '() 0 0 0))
  (check-pred loop-counters? ctrs)

  ;; Construct iteration-snapshot with ws=#f
  (define snap (iteration-snapshot ctrs #f cfg #f 10 100))
  (check-pred iteration-snapshot? snap)
  (check-equal? (iteration-snapshot-ws snap) #f)
  (check-equal? (iteration-snapshot-max-iterations snap) 10))

(test-case "T-03: iteration-snapshot round-trips through TR boundary"
  (define cfg (hash->session-config (hasheq 'max-iterations 5)))
  (define ctrs (loop-counters 1 2 '() 3 4 '() 5 6 7))
  (define snap (iteration-snapshot ctrs #f cfg #f 5 50))
  ;; Access all fields to exercise the TR boundary
  (check-pred loop-counters? (iteration-snapshot-counters snap))
  (check-false (iteration-snapshot-ws snap))
  (check-pred session-config? (iteration-snapshot-config snap))
  (check-equal? (iteration-snapshot-max-iterations snap) 5))
