#lang racket/base

;; BUG-0037 v1.00.20 W1 — mutation-stall watchdog v2: a stall is
;; REPETITION, not the mere absence of mutation.
;;
;;   * 70 DISTINCT reads → never trip (healthy exploration survives).
;;   * same call signature ≥3× within the window → hard-stall
;;     (soft-steer at 2, latched once).
;;   * absolute backstop (200) still kills signature-cycling livelocks.
;;   * a mutation resets the counter AND clears the repetition window.
;;   * snapshot carries stall-reason / stall-tool / recent-tools so kill
;;     messages tell operators what looped.

(require rackunit
         (only-in rackunit/text-ui run-tests)
         (only-in "../extensions/gsd/wave-executor.rkt"
                  make-stall-watchdog
                  stall-watchdog-observe!
                  stall-watchdog-snapshot
                  STALL-SOFT-LIMIT-DEFAULT
                  STALL-HARD-LIMIT-DEFAULT
                  STALL-REPETITION-WINDOW-DEFAULT
                  STALL-BACKSTOP-LIMIT-DEFAULT
                  tool-call-signature))

;; A read of a DISTINCT path per index (distinct signatures).
(define (read-call [i 0])
  (hasheq 'name 'read 'arguments (hasheq 'path (format "/tmp/file-~a.rkt" i))))

;; The SAME read repeated (one signature).
(define (same-read)
  (read-call 7))

(define (same-reads-batch n)
  (build-list n (lambda (_) (same-read))))

(define (mutation-call)
  (hasheq 'name 'write 'arguments (hasheq 'path "/tmp/out.rkt")))

(define (observe-each wd mk n)
  (for/list ([i (in-range n)])
    (stall-watchdog-observe! wd (list (mk i)))))

(define suite
  (test-suite "BUG-0037 W1: repetition-based stall detection"

    (test-case "70 DISTINCT reads never trip (the pre-fix W5 death is impossible)"
      (define wd (make-stall-watchdog))
      (define out (observe-each wd read-call 70))
      (check-false (memq 'hard-stall out) "distinct reads must NEVER accumulate toward a kill")
      (check-false (memq 'soft-stall out)))

    (test-case "identical read repeated: soft steer at 5, hard kill at 8"
      (define wd (make-stall-watchdog))
      (for ([i (in-range 4)])
        (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'ok))
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'soft-stall)
      (define snap (stall-watchdog-snapshot wd))
      (check-eq? (hash-ref snap 'stall-reason) 'repetition)
      (check-eq? (hash-ref snap 'stall-repeats) 5)
      (check-equal? (hash-ref snap 'stall-tool) "read")
      ;; Latched steering; counts 6 and 7 stay ok, 8 kills.
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'ok)
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'ok)
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'hard-stall)
      (check-eq? (hash-ref (stall-watchdog-snapshot wd) 'stall-repeats) 8))

    (test-case "enough distinct reads age a repeated signature out of the window"
      ;; window=10: repeat one read twice, then 10 distinct reads — BOTH
      ;; occurrences fall out of the newest-10 window, no trip.
      (define wd (make-stall-watchdog))
      ;; Latch the soft steer with 5 identical repeats first.
      (for ([i (in-range 4)])
        (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'ok))
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'soft-stall)
      ;; offset 100: genuinely disjoint from same-read's file-7 path.
      ;; window=20 → 20 distinct reads flush ALL occurrences.
      (define out (observe-each wd (lambda (i) (read-call (+ 100 i))) 20))
      (check-false (memq 'hard-stall out))
      (check-eq? (stall-watchdog-observe! wd (list (same-read)))
                 'ok
                 "signature aged out of the window — fresh start"))
    (test-case "two repeats plus fewer-than-window distinct reads stay dangerous"
      ;; With default limits (5/8) a third repeat is NOT a kill — only
      ;; escalation to ≥8 within the newest-20 window dies. Pin that:
      (define wd (make-stall-watchdog))
      (stall-watchdog-observe! wd (list (same-read)))
      (stall-watchdog-observe! wd (list (same-read)))
      (define out (observe-each wd (lambda (i) (read-call (+ 200 i))) 15))
      (check-eq? (stall-watchdog-observe! wd (list (same-read)))
                 'ok
                 "a third repeat is steered territory, not death"))
    (test-case "small explicit window keeps tight-repetition semantics"
      ;; A coordinator may opt into a tighter window; pin the decay rule
      ;; (cap drops the OLDEST occurrence first).
      (define wd (make-stall-watchdog #:window 10 #:soft-limit 3 #:hard-limit 3))
      (stall-watchdog-observe! wd (list (same-read)))
      (stall-watchdog-observe! wd (list (same-read)))
      (define out (observe-each wd (lambda (i) (read-call (+ 300 i))) 7))
      (check-false (memq 'hard-stall out))
      (check-eq? (stall-watchdog-observe! wd (list (same-read)))
                 'hard-stall
                 "3rd occurrence inside the newest-10 window kills"))

    (test-case "backstop kills signature-cycling livelocks at 200 mutation-free calls"
      (define wd (make-stall-watchdog #:window #f))
      ;; window disabled → only the backstop can fire; cycle signatures.
      (define out '())
      (define tripped
        (let loop ([i 0])
          (cond
            [(>= i 200) #f]
            [else
             (define r (stall-watchdog-observe! wd (list (read-call i))))
             (set! out (cons r out))
             (if (eq? r 'hard-stall)
                 #t
                 (loop (add1 i)))])))
      (check-true tripped "cycling signatures must die at the backstop")
      (check-eq? (hash-ref (stall-watchdog-snapshot wd) 'stall-reason) 'backstop)
      (check-equal? (length out) 200))

    (test-case "a mutation resets calls-since-mutation AND clears the window"
      (define wd (make-stall-watchdog))
      (stall-watchdog-observe! wd (list (same-read)))
      (stall-watchdog-observe! wd (list (same-read))) ; soft latched
      (stall-watchdog-observe! wd (list (mutation-call)))
      (define snap (stall-watchdog-snapshot wd))
      (check-equal? (hash-ref snap 'calls-since-mutation) 0)
      (check-equal? (hash-ref snap 'mutations) 1)
      (check-equal? (hash-ref snap 'window) '() "progress invalidates repetition evidence")
      ;; Post-mutation identical repeats restart from a clean slate.
      (check-eq? (stall-watchdog-observe! wd (list (same-read))) 'ok))

    (test-case "recent-tools names the last distinct tools for kill messages"
      (define wd (make-stall-watchdog))
      (stall-watchdog-observe! wd (list (read-call 0)))
      (stall-watchdog-observe! wd (list (mutation-call)))
      (stall-watchdog-observe! wd (list (hasheq 'name 'grep 'arguments (hasheq 'pattern "x"))))
      (define tools (hash-ref (stall-watchdog-snapshot wd) 'recent-tools))
      (check-equal? tools '(grep write read) "newest-first, distinct, includes mutations"))

    (test-case "signatures distinguish tool name and arguments"
      (check-false (equal? (tool-call-signature (read-call 1)) (tool-call-signature (read-call 2)))
                   "different paths → different signatures")
      (check-true (equal? (tool-call-signature (read-call 7)) (tool-call-signature (same-read)))
                  "same tool + same args → same signature"))

    (test-case "default limits are the documented v2 values"
      (check-equal? STALL-SOFT-LIMIT-DEFAULT 5)
      (check-equal? STALL-HARD-LIMIT-DEFAULT 8)
      (check-equal? STALL-REPETITION-WINDOW-DEFAULT 20)
      (check-equal? STALL-BACKSTOP-LIMIT-DEFAULT 200))

    (test-case "LIVE REGRESSION: repeated grep/test re-runs between reads stay alive"
      ;; v1.00.20 W2 attempt 1 died at 4 calls: 3 identical greps tripped
      ;; hard-limit 3. Legitimate work repeats a call a handful of times
      ;; while working through results — that must NEVER kill.
      (define wd (make-stall-watchdog))
      (define (grep-call)
        (hasheq 'name 'grep 'arguments (hasheq 'pattern "stall" 'path "extensions/gsd")))
      (define results '())
      (for ([i (in-range 6)])
        (set!
         results
         (append
          results
          (list (stall-watchdog-observe! wd (list (grep-call)))
                (stall-watchdog-observe! wd (list (read-call i)))
                (stall-watchdog-observe!
                 wd
                 (list (hasheq 'name 'bash 'arguments (hasheq 'command "racket tests/x.rkt"))))))))
      ;; Interleaved legit repetition: at most ONE non-fatal soft steer,
      ;; NEVER a kill.
      (check-false (memq 'hard-stall results)
                   "interleaved legitimate repetition must not escalate to a kill")
      (check-equal? (length (filter (lambda (r) (eq? r 'soft-stall)) results)) 1))

    (test-case "#f limits disable their channel; fully inert watchdog"
      (define wd-none (make-stall-watchdog #:soft-limit #f #:hard-limit #f #:backstop #f))
      (check-eq? (stall-watchdog-observe! wd-none (list (same-read))) 'ok)
      (check-eq? (stall-watchdog-observe! wd-none (list (same-read))) 'ok)
      (check-eq? (stall-watchdog-observe! wd-none (list (same-read))) 'ok)
      (check-eq? (stall-watchdog-observe! wd-none (list (same-read))) 'ok)
      (define wd-no-hard (make-stall-watchdog #:hard-limit #f #:backstop #f))
      (for ([i (in-range 4)])
        (check-eq? (stall-watchdog-observe! wd-no-hard (list (same-read))) 'ok))
      (check-eq? (stall-watchdog-observe! wd-no-hard (list (same-read))) 'soft-stall)
      (check-eq? (stall-watchdog-observe! wd-no-hard (list (same-read)))
                 'ok
                 "no hard limit → steer once, keep going"))

    (test-case "batch observation classifies by worst outcome in the batch"
      (define wd (make-stall-watchdog))
      ;; A single batch of 92 distinct reads: healthy under v2.
      (check-eq? (stall-watchdog-observe! wd
                                          (for/list ([i (in-range 92)])
                                            (read-call i)))
                 'ok)
      ;; A batch that crosses the hard limit inside itself: hard.
      (define wd2 (make-stall-watchdog))
      (stall-watchdog-observe! wd2 (list (same-read)))
      (check-eq? (stall-watchdog-observe! wd2 (same-reads-batch 7)) 'hard-stall))))

(module+ main
  (exit (run-tests suite)))
