#lang racket/base

;; q/tests/test-gsd-mutation-stall-watchdog.rkt — v1.00.18 W5 (#9513)
;;
;; Mutation-stall watchdog: mid-session steering for wave executors.
;; v1.00.16 W3 attempt-2 made 92 read-only tool calls, never edited a
;; file, and nothing noticed until delivery verification (~40 min, ~$12).
;;
;; Covered, per the wave spec:
;;   1. stall-state — pure fold over synthetic tool-call records.
;;   2. make-stall-watchdog + stall-watchdog-observe! — soft inject at 25
;;      (defaults), hard fail at 60 (defaults), latched soft injection.
;;   3. A healthy editing session is never interrupted.
;;   4. #f limits are inert (opt-out + tests).
;;   5. Mutation classification: write/edit/racket_edit/planning-write and
;;      racket_codemod(write=true) reset the counter; reads/greps/bash do
;;      NOT (even a mutating bash call — during implementation, file-tool
;;      mutations are the deliverable signal).
;;   6. Orchestrator integration: stall-steering-message reuses W2's
;;      re-anchor constructor so role context travels with the steering;
;;      stall-hard-failure-message is an honest, non-infra failure cause;
;;      current-gsd-stall-steerer is bindable (tests / adapters).

(require racket/string
         rackunit
         "../extensions/gsd/wave-executor.rkt"
         "../extensions/gsd/go-orchestrator.rkt")

;; ---- helpers ----------------------------------------------------------

(define (call name [arguments #f])
  (if arguments
      (hasheq 'name name 'arguments arguments)
      (hasheq 'name name)))

(define (reads n)
  (build-list n (lambda (_) (call 'read))))

(define (outcome-at soft hard records)
  (define wd (make-stall-watchdog #:soft-limit soft #:hard-limit hard))
  (for ([batch (in-list records)])
    (define result (stall-watchdog-observe! wd batch))
    (when (eq? result 'hard-stall)
      (error 'outcome-at "hard-stall hit early at batch")))
  wd)

;; ============================================================
;; 1. Defaults are what the module header documents (#9513)
;; ============================================================

(test-case "default soft limit is 25 and hard limit is 60"
  (check-equal? STALL-SOFT-LIMIT-DEFAULT 25)
  (check-equal? STALL-HARD-LIMIT-DEFAULT 60))

(test-case "run-campaign-wave exposes the limits as keyword arguments"
  ;; Contract-level check: the coordinator passes #:stall-soft-limit /
  ;; #:stall-hard-limit. Verify the keywords are accepted and #f disables.
  (define-values (soft-sym hard-sym)
    (values (string->symbol "stall-soft-limit") (string->symbol "stall-hard-limit")))
  (check-pred symbol? soft-sym)
  (check-pred symbol? hard-sym)
  ;; #f must be a legal limit value for both (opt-out).
  (check-true (stall-limit? #f))
  (check-true (stall-limit? 1))
  (check-false (stall-limit? 0))
  (check-false (stall-limit? -3))
  (check-false (stall-limit? 1.5))
  (check-false (stall-limit? "25")))

;; ============================================================
;; 2. stall-state — pure fold over synthetic sequences
;; ============================================================

(test-case "stall-state: all reads → every call counts as stalled"
  (define st (stall-state (reads 30)))
  (check-equal? (hash-ref st 'calls-since-mutation) 30)
  (check-equal? (hash-ref st 'total-calls) 30)
  (check-equal? (hash-ref st 'mutations) 0))

(test-case "stall-state: a mutation resets calls-since-mutation to 0"
  (define st (stall-state (append (reads 24) (list (call 'edit)) (reads 5))))
  (check-equal? (hash-ref st 'calls-since-mutation) 5)
  (check-equal? (hash-ref st 'total-calls) 30)
  (check-equal? (hash-ref st 'mutations) 1))

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
;; 3. Watchdog: soft inject at 25, hard fail at 60 (defaults)
;; ============================================================

(test-case "soft limit fires exactly once at 25 read-only calls"
  (define wd (make-stall-watchdog))
  ;; 24 reads: still ok.
  (check-eq? (stall-watchdog-observe! wd (reads 24)) 'ok)
  (define snap-24 (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap-24 'calls-since-mutation) 24)
  (check-false (hash-ref snap-24 'soft-sent?))
  ;; 25th read: soft-stall, and the injection latches.
  (check-eq? (stall-watchdog-observe! wd (reads 1)) 'soft-stall)
  (check-true (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?))
  ;; More read-only calls: never re-injected (one soft injection per
  ;; session, not per call).
  (check-eq? (stall-watchdog-observe! wd (reads 10)) 'ok)
  (check-eq? (stall-watchdog-observe! wd (reads 20)) 'ok))

(test-case "hard limit fails the attempt at 60 calls without a mutation"
  (define wd (make-stall-watchdog))
  ;; 59 read-only calls: steered once at 25, still running.
  (check-eq? (stall-watchdog-observe! wd (reads 59)) 'soft-stall)
  ;; 60th read: hard-stall — the exploring executor must fail honestly.
  (check-eq? (stall-watchdog-observe! wd (reads 1)) 'hard-stall)
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'calls-since-mutation) 60)
  (check-equal? (hash-ref snap 'total-calls) 60)
  (check-equal? (hash-ref snap 'mutations) 0)
  (check-true (hash-ref snap 'soft-sent?)))

(test-case "hard limit wins when an executor is deep past both limits"
  ;; The very first observation already carries 92 exploring calls (the
  ;; v1.00.16 W3 attempt-2 shape): it must hard-fail, not be re-steered.
  (define wd (make-stall-watchdog))
  (check-eq? (stall-watchdog-observe! wd (reads 92)) 'hard-stall))

(test-case "an edit between observations resets the stall counter"
  (define wd (make-stall-watchdog))
  (check-eq? (stall-watchdog-observe! wd (reads 25)) 'soft-stall)
  ;; The executor heeds the steering and edits.
  (check-eq? (stall-watchdog-observe! wd (list (call 'edit))) 'ok)
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'calls-since-mutation) 0)
  (check-equal? (hash-ref snap 'mutations) 1)
  ;; Post-steering reads start from a clean slate; no re-injection.
  (check-eq? (stall-watchdog-observe! wd (reads 24)) 'ok)
  (check-true (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?)))

;; ============================================================
;; 4. A healthy editing session is never interrupted
;; ============================================================

(test-case "interleaved read/edit work never reaches either limit"
  ;; 120 total calls, but a file mutation every ~10 calls: since-counter
  ;; never exceeds 10, far below soft (25) and hard (60). Each batch leads
  ;; with the mutation, so the final snapshot legitimately reads 9.
  (define records
    (apply append (build-list 12 (lambda (i) (cons (call (if (even? i) 'write 'edit)) (reads 9))))))
  (define wd (outcome-at STALL-SOFT-LIMIT-DEFAULT STALL-HARD-LIMIT-DEFAULT (map list records)))
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'mutations) 12)
  (check-equal? (hash-ref snap 'calls-since-mutation) 9)
  (check-false (hash-ref snap 'soft-sent?)))

(test-case "edit-first session: no soft injection, no hard failure"
  (define wd (make-stall-watchdog))
  (check-eq? (stall-watchdog-observe! wd (list (call 'edit))) 'ok)
  ;; 24 read calls stay under the soft limit (25) — the session is
  ;; healthy precisely because reads never accumulate to the limit.
  (check-eq? (stall-watchdog-observe! wd (reads 24)) 'ok)
  (check-eq? (stall-watchdog-observe! wd (list (call 'write))) 'ok)
  (check-eq? (stall-watchdog-observe! wd (reads 24)) 'ok)
  (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'mutations) 2))

;; ============================================================
;; 5. Disabled limits are inert
;; ============================================================

(test-case "#f soft limit disables steering only"
  (define wd (make-stall-watchdog #:soft-limit #f))
  (check-false (stall-watchdog-soft-limit wd))
  (check-equal? (stall-watchdog-hard-limit wd) STALL-HARD-LIMIT-DEFAULT)
  ;; 40 read-only calls: past the (disabled) soft limit, no injection...
  (check-eq? (stall-watchdog-observe! wd (reads 40)) 'ok)
  (check-false (hash-ref (stall-watchdog-snapshot wd) 'soft-sent?))
  ;; ...but the hard limit still fails at 60.
  (check-eq? (stall-watchdog-observe! wd (reads 20)) 'hard-stall))

(test-case "#f hard limit disables termination only"
  (define wd (make-stall-watchdog #:hard-limit #f))
  (check-eq? (stall-watchdog-observe! wd (reads 25)) 'soft-stall)
  ;; 200 read-only calls: steered once, never terminated.
  (check-eq? (stall-watchdog-observe! wd (reads 175)) 'ok)
  (define snap (stall-watchdog-snapshot wd))
  (check-equal? (hash-ref snap 'calls-since-mutation) 200)
  (check-true (hash-ref snap 'soft-sent?)))

(test-case "both limits #f make the watchdog fully inert"
  (define wd (make-stall-watchdog #:soft-limit #f #:hard-limit #f))
  (check-eq? (stall-watchdog-observe! wd (reads 500)) 'ok)
  (check-equal? (hash-ref (stall-watchdog-snapshot wd) 'total-calls) 500))

(test-case "make-stall-watchdog rejects invalid limits"
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:soft-limit 0)))
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:hard-limit -1)))
  (check-exn exn:fail:contract? (lambda () (make-stall-watchdog #:soft-limit "25"))))

;; ============================================================
;; 6. Orchestrator integration (steering + honest failure)
;; ============================================================

(test-case "stall-steering-message carries the executor role and the order"
  (define msg
    (stall-steering-message 25
                            "W3"
                            "camp-42"
                            "Add the stall watchdog"
                            '("q/extensions/gsd/wave-executor.rkt"
                              "q/tests/test-gsd-mutation-stall-watchdog.rkt")))
  ;; W2's re-anchor constructor travels with it: role, ids, task.
  (check-pred (lambda (s) (string-contains? s "W3")) msg)
  (check-pred (lambda (s) (string-contains? s "camp-42")) msg)
  (check-pred (lambda (s) (string-contains? s "Add the stall watchdog")) msg)
  ;; The watchdog payload: count + targets + the imperative.
  (check-pred (lambda (s) (string-contains? s "25")) msg)
  (check-pred (lambda (s) (string-contains? s "q/extensions/gsd/wave-executor.rkt")) msg)
  (check-pred (lambda (s) (string-contains? s "q/tests/test-gsd-mutation-stall-watchdog.rkt")) msg)
  (check-pred (lambda (s) (string-contains? (string-downcase s) "edit")) msg))

(test-case "stall-steering-message degrades without target files"
  (define msg (stall-steering-message 30 "W2" "camp-42" "Do the thing" '()))
  (check-pred (lambda (s) (string-contains? s "30")) msg)
  (check-pred string? msg))

(test-case "stall-hard-failure-message is an honest, non-infra cause"
  (define msg (stall-hard-failure-message 60 60 '("q/extensions/gsd/wave-executor.rkt")))
  (check-pred (lambda (s) (string-contains? s "mutation-stall watchdog")) msg)
  (check-pred (lambda (s) (string-contains? s "60")) msg)
  (check-pred (lambda (s) (string-contains? s "q/extensions/gsd/wave-executor.rkt")) msg)
  ;; Must NOT read as a transient provider/infra failure: the attempt
  ;; consumes its failure honestly (D8 / #9357 classification).
  (for ([banned '("network" "connection" "stream" "timeout" "provider" "retry")])
    (check-false (string-contains? (string-downcase msg) banned)
                 (format "hard-failure message must not mention ~a" banned))))

(test-case "gsd-stall-exn is a transparent failure exception"
  (define e (make-gsd-stall-exn "mutation-stall watchdog: boom"))
  (check-pred gsd-stall-exn? e)
  (check-pred exn:fail? e)
  (check-equal? (exn-message e) "mutation-stall watchdog: boom")
  ;; The wrapper converts it into a 'failed outcome with that message.
  (check-true (string-contains? (exn-message e) "mutation-stall")))

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
  ;; The go-orchestrator provide list must expose the W5 seam so the
  ;; campaign runner (and these tests) can reach it.
  (check-pred procedure? stall-steering-message)
  (check-pred procedure? stall-hard-failure-message)
  (check-pred procedure? wave-doc-target-files)
  (check-pred procedure? wrap-run-one-with-stall-watchdog)
  (check-pred parameter? current-gsd-stall-steerer)
  (check-true (stall-watchdog? (make-stall-watchdog))))

;; Neutral completion line: rackunit prints any failures itself and the
;; process exit code is the authoritative pass/fail signal.
(displayln "test-gsd-mutation-stall-watchdog: test run complete (exit code is authoritative)")
