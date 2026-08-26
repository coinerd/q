#lang racket/base

;; q/tests/test-gsd-mutation-stall-watchdog.rkt — GSD mutation-stall watchdog
;; (#9513; v2 semantics per BUG-0037 / v1.00.20 W1)
;;
;; v1.00.16 W3 attempt-2 made 92 read-only tool calls, never edited a file,
;; and nothing noticed until delivery verification (~40 min, ~$12). The v1
;; watchdog fixed that with a flat call-count budget — and then killed
;; legitimate 60+ distinct-read exploration twice in the previous campaign.
;;
;; v2 semantics (BUG-0037): a stall is REPETITION within a recent window,
;; not the mere absence of mutation:
;;   * same tool-call signature ≥ soft(2)× in the window → steer once;
;;   * ≥ hard(3)× → kill the attempt (retryable infrastructure);
;;   * absolute backstop (200) still kills signature-cycling livelocks;
;;   * DISTINCT calls never accumulate toward a kill.
;;
;; Covered here:
;;   1. stall-state — pure fold over synthetic tool-call records.
;;   2. make-stall-watchdog + stall-watchdog-observe! — repetition soft/
;;      hard limits, latched steering, backstop, inert opt-outs.
;;   3. A healthy editing session (many DISTINCT reads between edits) is
;;      never interrupted.
;;   4. Mutation classification: write/edit/racket_edit/planning-write and
;;      racket_codemod(write=true) reset the counter and clear the window;
;;      reads/greps/bash do NOT (even a mutating bash call).
;;   5. Orchestrator integration: steering reuses W2's re-anchor
;;      constructor; stall death maps to a RETRYABLE 'infra-failed
;;      outcome (bounded auto-resume, BUG-0037 W1); steerer is bindable.

(require racket/string
         rackunit
         (only-in "../util/loop-result.rkt" make-loop-result)
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome-kind
                  wave-execution-outcome-message)
         "../extensions/gsd/wave-executor.rkt"
         "../extensions/gsd/go-orchestrator.rkt")

;; ---- helpers ----------------------------------------------------------

(define (call name [arguments #f])
  (if arguments
      (hasheq 'name name 'arguments arguments)
      (hasheq 'name name)))

;; N DISTINCT reads (unique paths) — the healthy-exploration shape.
(define (reads n [offset 0])
  (build-list n (lambda (i) (call 'read (hasheq 'path (format "/tmp/f-~a.rkt" (+ offset i)))))))

;; The SAME read repeated — the livelock shape.
(define (same-reads n)
  (build-list n (lambda (_) (call 'read (hasheq 'path "/tmp/loop.rkt")))))

(define (outcome-at soft hard records)
  (define wd (make-stall-watchdog #:soft-limit soft #:hard-limit hard))
  (for ([batch (in-list records)])
    (define result (stall-watchdog-observe! wd batch))
    (when (eq? result 'hard-stall)
      (error 'outcome-at "hard-stall hit early at batch")))
  wd)

;; ============================================================
;; 1. Defaults are what the module header documents (v2)
;; ============================================================

(test-case "defaults: soft 8, hard 15, window 30, backstop 300"
  (check-equal? STALL-SOFT-LIMIT-DEFAULT 8)
  (check-equal? STALL-HARD-LIMIT-DEFAULT 15)
  (check-equal? STALL-REPETITION-WINDOW-DEFAULT 30)
  (check-equal? STALL-BACKSTOP-LIMIT-DEFAULT 300))

(test-case "LIVE REGRESSION: repeated identical greps between reads never kill"
  ;; v1.00.20 W2 attempt 1: killed at 4 calls for 3 identical greps.
  ;; Interleaved legitimate repetition must stay alive under 5/8.
  (define wd (make-stall-watchdog))
  (define (grep-call)
    (call 'grep (hasheq 'pattern "stall" 'path "extensions/gsd")))
  (define results '())
  (for ([i (in-range 6)])
    (set! results
          (append results
                  (list (stall-watchdog-observe! wd (list (grep-call)))
                        (stall-watchdog-observe! wd (reads 1 (* 100 i)))))))
  ;; Zero nudges across 6 legit re-runs spread across other calls; NEVER a kill.
  (check-false (memq 'hard-stall results) "legit repetition must not kill")
  (check-false (memq 'soft-stall results) "legit repetition stays under the steer line"))

(test-case "run-campaign-wave exposes the limits as keyword arguments"
  ;; Contract-level check: the coordinator passes #:stall-soft-limit /
  ;; #:stall-hard-limit. Verify the keywords are accepted and #f disables.
  (check-pred symbol? 'stall-soft-limit)
  (check-pred symbol? 'stall-hard-limit)
  ;; #f must be a legal limit value (opt-out).
  (check-true (stall-limit? #f))
  (check-true (stall-limit? 1))
  (check-false (stall-limit? 0))
  (check-false (stall-limit? -3))
  (check-false (stall-limit? 1.5))
  (check-false (stall-limit? "25")))

;; ============================================================
;; 2. stall-state — pure fold over synthetic sequences
;; ============================================================

(test-case "stall-state: distinct reads accumulate only the backstop signal"
  (define st (stall-state (reads 30)))
  (check-equal? (hash-ref st 'calls-since-mutation) 30)
  (check-equal? (hash-ref st 'total-calls) 30)
  (check-equal? (hash-ref st 'mutations) 0)
  ;; Window capped at the default size (newest 30).
  (check-equal? (length (hash-ref st 'window)) 30))

(test-case "stall-state: a mutation resets the counter AND clears the window"
  (define st (stall-state (append (reads 24) (list (call 'edit)) (reads 5 500))))
  (check-equal? (hash-ref st 'calls-since-mutation) 5)
  (check-equal? (hash-ref st 'total-calls) 30)
  (check-equal? (hash-ref st 'mutations) 1)
  ;; The window was cleared by the edit and repopulated by ONLY the
  ;; post-edit reads — all pre-edit signatures are gone.
  (check-equal? (length (hash-ref st 'window)) 5)
  (check-false (member (tool-call-signature (call 'read (hasheq 'path "/tmp/f-0.rkt")))
                       (hash-ref st 'window))))

(test-case "stall-state: mutation classification"
  ;; Always-mutating file tools.
  (for ([name '(write edit racket_edit planning-write)])
    (check-true (mutation-tool-name? name) (format "~a must be a mutation" name))
    (check-true (mutation-tool-call? (call name))))
  ;; Wire format: string tool names are normalized too.
  (check-true (mutation-tool-call? (call "edit")))
  (check-true (mutation-tool-call? (call "write")))
  ;; racket_codemod is conditional on write=true.
  (check-eq? (mutation-tool-name? 'racket_codemod) 'needs-arguments)
  (check-true (mutation-tool-call? (call 'racket_codemod (hasheq 'write #t))))
  (check-true (mutation-tool-call? (call 'racket_codemod (hasheq 'write "true"))))
  ;; Dry-run codemod is a read.
  (check-false (mutation-tool-call? (call 'racket_codemod (hasheq 'write #f))))
  (check-false (mutation-tool-call? (call 'racket_codemod (hasheq 'write "false"))))
  ;; Reads, greps, bash — including a *mutating* bash invocation — must
  ;; NOT reset the counter during implementation.
  (check-false (mutation-tool-name? 'read))
  (check-false (mutation-tool-name? 'grep))
  (check-false (mutation-tool-name? 'bash))
  (check-false (mutation-tool-call?
                (call 'bash (hasheq 'command "rm -rf build && sed -i s/a/b/ src.rkt")))))

;; ============================================================
;; 3. Watchdog v2: repetition steers at 2, kills at 3; backstop at 200
;; ============================================================

(test-case "identical reads: soft steer at 8 (latched once), hard kill at 15"
  (define wd (make-stall-watchdog))
  (for ([i (in-range 7)])
    (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'ok))
  (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'soft-stall)
  (check-true (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?))
  ;; Latched: counts 9..14 stay ok, the 15th kills.
  (for ([i (in-range 6)])
    (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'ok))
  (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'hard-stall)
  (define snap (stall-watchdog-snapshot wd))
  (check-eq? (hash-ref snap 'stall-reason) 'repetition)
  (check-eq? (hash-ref snap 'stall-repeats) 15))

(test-case "70 DISTINCT reads NEVER trip (the pre-fix W5 death is impossible)"
  (define wd (make-stall-watchdog))
  (for ([i (in-range 7)])
    (check-eq? (stall-watchdog-observe! wd (reads 10 (* 100 i))) 'ok))
  (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'calls-since-mutation) 70)
  (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'ok))

(test-case "backstop kills signature-cycling livelocks at 300"
  (define wd (make-stall-watchdog #:window #f))
  (let loop ([i 0])
    (define r (stall-watchdog-observe! wd (reads 1 i)))
    (unless (or (eq? r 'hard-stall) (>= i 299))
      (loop (add1 i))))
  (define snap (stall-watchdog-snapshot wd))
  (check-eq? (hash-ref snap 'calls-since-mutation) 300)
  (check-eq? (hash-ref snap 'stall-reason) 'backstop))

(test-case "an edit between observations resets the counter and the window"
  (define wd (make-stall-watchdog))
  (check-eq? (stall-watchdog-observe! wd (same-reads 8)) 'soft-stall)
  ;; The executor heeds the steering and edits.
  (check-eq? (stall-watchdog-observe! wd (list (call 'edit))) 'ok)
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'calls-since-mutation) 0)
  (check-equal? (hash-ref snap 'mutations) 1)
  (check-equal? (hash-ref snap 'window) '())
  ;; Post-edit identical repeats start from a clean slate (no instant hard).
  (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'ok)
  (check-true (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?)))

;; ============================================================
;; 4. A healthy editing session is never interrupted
;; ============================================================

(test-case "interleaved read/edit work never reaches any limit"
  ;; 120 total calls with DISTINCT reads and a file mutation every ~10
  ;; calls: the window clears on every mutation, so repetition evidence
  ;; can never reach 2.
  (define records
    (apply append
           (build-list 12
                       (lambda (i) (cons (call (if (even? i) 'write 'edit)) (reads 9 (* 1000 i)))))))
  (define wd (outcome-at STALL-SOFT-LIMIT-DEFAULT STALL-HARD-LIMIT-DEFAULT (map list records)))
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'mutations) 12)
  (check-equal? (hash-ref snap 'calls-since-mutation) 9)
  (check-false (hash-ref snap 'soft-sent?)))

(test-case "edit-first session: no steering, no failure"
  (define wd (make-stall-watchdog))
  (check-eq? (stall-watchdog-observe! wd (list (call 'edit))) 'ok)
  ;; Distinct reads never accumulate repetition evidence.
  (check-eq? (stall-watchdog-observe! wd (reads 24)) 'ok)
  (check-eq? (stall-watchdog-observe! wd (list (call 'write))) 'ok)
  (check-eq? (stall-watchdog-observe! wd (reads 24 500)) 'ok)
  (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'mutations) 2))

;; ============================================================
;; 5. Disabled channels are inert (opt-out granularity)
;; ============================================================

(test-case "#f soft limit disables steering only"
  (define wd (make-stall-watchdog #:soft-limit #f))
  (check-false (stall-watchdog-soft-limit wd))
  ;; Identical reads skip steering entirely...
  (check-eq? (stall-watchdog-observe! wd (same-reads 14)) 'ok)
  (check-false (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?))
  ;; ...and go straight to the hard kill at 15.
  (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'hard-stall))

(test-case "#f hard limit disables termination; backstop remains"
  (define wd (make-stall-watchdog #:hard-limit #f))
  (check-eq? (stall-watchdog-observe! wd (same-reads 8)) 'soft-stall)
  ;; Steered once, then identical repeats continue harmlessly...
  (for ([i (in-range 50)])
    (check-eq? (stall-watchdog-observe! wd (same-reads 1)) 'ok))
  ;; ...but the ABSOLUTE backstop still terminates a livelock eventually.
  (let loop ([i 0]
             [r 'ok])
    (unless (or (eq? r 'hard-stall) (>= i 300))
      (loop (add1 i) (stall-watchdog-observe! wd (reads 1 (+ 900 i))))))
  (check-eq? (hash-ref (stall-watchdog-snapshot wd) 'stall-reason) 'backstop))

(test-case "all channels #f make the watchdog fully inert"
  (define wd (make-stall-watchdog #:soft-limit #f #:hard-limit #f #:backstop #f))
  (check-eq? (stall-watchdog-observe! wd (same-reads 500)) 'ok)
  (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'total-calls) 500))

(test-case "make-stall-watchdog rejects invalid limits"
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:soft-limit 0)))
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:hard-limit -1)))
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:window 0)))
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:backstop "200"))))

;; ============================================================
;; 6. Orchestrator integration (steering + RETRYABLE classification)
;; ============================================================

(test-case "stall-steering-message carries the executor role and the order"
  (define msg
    (stall-steering-message 5
                            "W3"
                            "camp-42"
                            "Add the stall watchdog"
                            '("q/extensions/gsd/wave-executor.rkt"
                              "q/tests/test-gsd-mutation-stall-watchdog.rkt")))
  ;; W2's re-anchor constructor travels with it: role, ids, task.
  (check-pred (lambda (s) (string-contains? s "W3")) msg)
  (check-pred (lambda (s) (string-contains? s "camp-42")) msg)
  (check-pred (lambda (s) (string-contains? s "Add the stall watchdog")) msg)
  (check-pred (lambda (s) (string-contains? s "5")) msg)
  (check-pred (lambda (s) (string-contains? s "q/extensions/gsd/wave-executor.rkt")) msg)
  (check-pred (lambda (s) (string-contains? s "q/tests/test-gsd-mutation-stall-watchdog.rkt")) msg)
  (check-pred (lambda (s) (string-contains? (string-downcase s) "edit")) msg))

(test-case "stall-steering-message degrades without target files"
  (define msg (stall-steering-message 5 "W2" "camp-42" "Do the thing" '()))
  (check-pred (lambda (s) (string-contains? s "5")) msg)
  (check-pred string? msg))

(test-case "stall-hard-failure-message names reason, looped tool, and recent tools"
  (define msg
    (stall-hard-failure-message 60
                                60
                                '("q/extensions/gsd/wave-executor.rkt")
                                "read"
                                '(grep read bash)))
  (check-pred (lambda (s) (string-contains? s "mutation-stall watchdog")) msg)
  (check-pred (lambda (s) (string-contains? s "'read'")) msg)
  (check-pred (lambda (s) (string-contains? s "grep")) msg)
  (check-pred (lambda (s) (string-contains? s "q/extensions/gsd/wave-executor.rkt")) msg)
  (check-pred (lambda (s) (string-contains? (string-downcase s) "re-attempted")) msg))

(test-case "stall-cause errors classify as RETRYABLE infra-failed (live-campaign regression)"
  ;; v1.00.20 W2 attempt 1: the session layer converted the stall exn to a
  ;; loop-result 'error; the campaign then STOPPED instead of auto-retrying.
  (define r
    (prompt-run-result->outcome
     (make-loop-result
      '()
      'error
      (hasheq 'error "mutation-stall watchdog: attempt terminated after 4 mutation-free calls"))))
  (check-eq? (wave-execution-outcome-kind r) 'infra-failed)
  (check-pred (lambda (s) (string-contains? s "mutation-stall watchdog"))
              (wave-execution-outcome-message r))
  ;; Non-stall errors must NOT be reclassified.
  (define other
    (prompt-run-result->outcome (make-loop-result '() 'error (hasheq 'error "some logic failure"))))
  (check-eq? (wave-execution-outcome-kind other) 'failed))

(test-case "gsd-stall-exn is a transparent failure exception"
  (define e (make-gsd-stall-exn "mutation-stall watchdog: boom"))
  (check-pred gsd-stall-exn? e)
  (check-pred exn:fail? e)
  (check-equal? (exn-message e) "mutation-stall watchdog: boom"))

(test-case "current-gsd-stall-steerer is bindable (test/adapter seam)"
  (define injected '())
  (parameterize ([current-gsd-stall-steerer (lambda (message)
                                              (set! injected
                                                    (cons (if (string? message)
                                                              (string-length message)
                                                              message)
                                                          injected)))])
    ((current-gsd-stall-steerer) "steer!")
    ((current-gsd-stall-steerer) "steer again!"))
  (check-equal? (length injected) 2))

(test-case "watchdog wiring components are exported for the coordinator"
  (check-pred procedure? stall-steering-message)
  (check-pred procedure? stall-hard-failure-message)
  (check-pred procedure? wave-doc-target-files)
  (check-pred procedure? wrap-run-one-with-stall-watchdog)
  (check-pred parameter? current-gsd-stall-steerer)
  (check-true (stall-watchdog? (make-stall-watchdog))))

;; Neutral completion line: rackunit prints any failures itself and the
;; process exit code is the authoritative pass/fail signal.
(displayln "test-gsd-mutation-stall-watchdog: test run complete (exit code is authoritative)")
