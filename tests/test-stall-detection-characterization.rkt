#lang racket/base

;; BUG-0037 characterization — mutation-stall watchdog counts CALLS, not
;; DISTINCT activity. Today:
;;   * 60+ DISTINCT read calls → 'hard-stall (kill signaled) — a genuine
;;     long exploration of many different files is killed at the same
;;     threshold as an identical-call loop.
;;   * repeated IDENTICAL calls → also 'hard-stall.
;; The owning fix wave flips these pins: distinct calls must be
;; distinguished from an identical-call loop.
;;
;; This file pins CURRENT behavior — every test PASSES today.

(require rackunit
         (only-in rackunit/text-ui run-tests)
         (only-in "../extensions/gsd/wave-executor.rkt"
                  make-stall-watchdog
                  stall-watchdog-observe!
                  stall-watchdog-snapshot
                  STALL-SOFT-LIMIT-DEFAULT
                  STALL-HARD-LIMIT-DEFAULT))

;; A read tool-call record: name only (no 'arguments needed unless the
;; tool is conditional like racket_codemod).
(define (read-call [i 0])
  (hasheq 'name 'read 'arguments (hasheq 'path (format "/tmp/file-~a.rkt" i))))

(define (mutation-call)
  (hasheq 'name 'write 'arguments (hasheq 'path "/tmp/out.rkt")))

;; Observe `n` calls one at a time; return list of classifications.
(define (observe-each wd mk n)
  (for/list ([i (in-range n)])
    (stall-watchdog-observe! wd (list (mk i)))))

(define-syntax check-final
  (syntax-rules ()
    [(_ wd expected msg) (check-equal? (stall-watchdog-observe! wd '()) expected msg)]))

(define suite
  (test-suite "BUG-0037: stall watchdog trips on raw call COUNT — distinctness ignored"

    (test-case "60+ DISTINCT read calls trip the hard limit today (kill signaled)"
      (define wd (make-stall-watchdog))
      (define out (observe-each wd read-call 59))
      (check-false (memq 'hard-stall out) "59 distinct reads must not yet hard-trip")
      (check-final wd 'ok "at 59 calls the watchdog must still be 'ok")
      ;; The 60th DISTINCT call crosses the hard limit.
      (check-equal? (stall-watchdog-observe! wd (list (read-call 59)))
                    'hard-stall
                    "the 60th distinct read must hard-trip today (trip-on-count semantics)"))

    (test-case "repeated IDENTICAL calls trip identically (no distinctness window)"
      (define wd (make-stall-watchdog))
      (define same (lambda (_) (read-call 7)))
      (define out (observe-each wd same 59))
      (check-false (memq 'hard-stall out))
      (check-equal? (stall-watchdog-observe! wd (list (same 0)))
                    'hard-stall
                    "an identical-call loop must hard-trip at the same count today"))

    (test-case "distinct vs identical are INDISTINGUISHABLE today (the defect)"
      (define wd-distinct (make-stall-watchdog))
      (define wd-loop (make-stall-watchdog))
      (for ([i (in-range 59)])
        (stall-watchdog-observe! wd-distinct (list (read-call i)))
        (stall-watchdog-observe! wd-loop (list (read-call 0))))
      (define snap-distinct (stall-watchdog-snapshot wd-distinct))
      (define snap-loop (stall-watchdog-snapshot wd-loop))
      (check-equal?
       (hash-ref snap-distinct 'calls-since-mutation)
       (hash-ref snap-loop 'calls-since-mutation)
       "BUG-0037 pin: the snapshot cannot distinguish 59 distinct reads from 59 identical reads — flip this when the fix adds a distinctness signal"))

    (test-case "a mutation resets the counter even amid distinct reads"
      (define wd (make-stall-watchdog))
      (for ([i (in-range 50)])
        (stall-watchdog-observe! wd (list (read-call i))))
      (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'calls-since-mutation) 50)
      (stall-watchdog-observe! wd (list (mutation-call)))
      (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'calls-since-mutation)
                    0
                    "a write resets calls-since-mutation today")
      (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'mutations) 1))

    (test-case "hard-stall outranks soft-stall when both limits are crossed"
      (define wd (make-stall-watchdog))
      ;; Feed all 70 at once: classification must be 'hard-stall, not a
      ;; late soft steer — an exploring executor deep past both limits
      ;; must fail today, not be re-steered.
      (define r
        (stall-watchdog-observe! wd
                                 (for/list ([i (in-range 70)])
                                   (read-call i))))
      (check-equal? r 'hard-stall)
      (check-true (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?)
                  "hard-stall latches soft-sent? too (no further steering)"))

    (test-case "no distinct-call tracking seam exists today (documented absent seam)"
      ;; The planned fix needs a distinctness window (distinct calls seen /
      ;; recent distinct signatures). Today the state carries none — this
      ;; is the seam BUG-0037's wave will add; its absence is pinned here.
      (define wd (make-stall-watchdog))
      (stall-watchdog-observe! wd (list (read-call 0)))
      (define st (stall-watchdog-snapshot wd))
      (check-false
       (ormap (lambda (k) (hash-ref st k #f))
              '(distinct-calls distinct-count recent-distinct distinct-window))
       "no distinctness key exists in the snapshot today; if one appears, BUG-0037's seam landed — flip this pin"))

    (test-case "default limits are 25/60 (fixture sanity)"
      (check-equal? STALL-SOFT-LIMIT-DEFAULT 25)
      (check-equal? STALL-HARD-LIMIT-DEFAULT 60))))

(module+ main
  (exit (run-tests suite)))
