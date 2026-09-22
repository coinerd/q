#lang racket/base
;; @covers extensions/gsd/wave-completion.rkt

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-gsd-wave-completion.rkt — W1: Verifier-First Completion and Lifecycle Truth
;;
;; TDD red tests for:
;;   1. Verifier rejection cannot persist DONE.
;;   2. Verifier approval persists DONE + outbox event.
;;   3. /skip commits DEFERRED durably.
;;   4. Duplicate completion events are deduplicated by stable event ID.
;;   5. Doc existence never implies completion (GC-5 regression).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  make-campaign-wave
                  make-campaign-record
                  campaign-manifest-hash
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-status!
                  set-campaign-fence-token!
                  begin-attempt!
                  select-next-actionable-wave
                  wave-failure-reason
                  attempt-failure-reason
                  stamp-wave-failure!
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign! load-campaign-record)
         (only-in "../extensions/gsd/delivery-handoff.rkt"
                  persist-delivery-handoff!
                  reconcile-delivered-handoff!)
         (only-in "../extensions/gsd/wave-completion.rkt"
                  try-complete-wave!
                  skip-wave!
                  completion-result-status
                  completion-result-event-id
                  count-completion-events
                  make-event-id
                  load-outbox))

;; W3 exports under test (red-first: dynamic until the module provides them).
(define-runtime-path handoff-module "../extensions/gsd/delivery-handoff.rkt")
(define-runtime-path wave-completion-module "../extensions/gsd/wave-completion.rkt")
(define delivery-handoff-status (dynamic-require handoff-module 'delivery-handoff-status))
(define reconcile-completion-outbox!
  (dynamic-require wave-completion-module 'reconcile-completion-outbox!))
(define completion-outbox-invariant?
  (dynamic-require wave-completion-module 'completion-outbox-invariant?))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tmp-campaign-dir n-waves)
  (define dir (make-temporary-file "wave-comp-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Completion\n\n## Waves\n\n" out)
                           (for ([i (in-range n-waves)])
                             (fprintf out "- [Inbox] W~a: Wave ~a → waves/W~a-wave.md\n" i i i)))
                         #:exists 'truncate)
  ;; BUG-0052: every referenced wave doc must exist for campaign creation.
  (for ([i (in-range n-waves)])
    (call-with-output-file
     (build-path dir ".planning" "waves" (format "W~a-wave.md" i))
     (lambda (out) (fprintf out "# Wave ~a\n\nGoal: wave ~a\n\n## Verify\n\nraco test .\n" i i))
     #:exists 'truncate))
  dir)

(define (load-or-migrate dir)
  (migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    w))

(define (wave-status* rec idx)
  (campaign-wave-status (wave* rec idx)))

(define (complete-current! dir rec idx approve?)
  (define attempt (campaign-wave-current-attempt (wave* rec idx)))
  (try-complete-wave! dir
                      rec
                      idx
                      #:verifier-approve? approve?
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt)))

;; Seed a canonical "Status: Inbox" header under the H1 so the lockstep
;; assertions observe real projection writes instead of a missing line.
(define (seed-status! dir idx)
  (define p (build-path dir ".planning" "waves" (format "W~a-wave.md" idx)))
  (define lines (string-split (call-with-input-file p port->string) "\n"))
  (call-with-output-file
   p
   (lambda (out)
     (display (string-join (append (list (car lines) "Status: Inbox") (cdr lines)) "\n") out))
   #:exists 'truncate))

;; ============================================================
;; Test suites
;; ============================================================

(define verifier-first-suite
  (test-suite "verifier-first completion"

    (test-case "verifier rejection cannot persist DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir rec 0 #f))
      (check-eq? (wave-status* rec 0) 'failed "rejected verifier marks wave 'failed, not 'done")
      (check-eq? (completion-result-status result) 'failed)
      ;; Verify it was persisted
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* loaded 0) 'failed)
      (cleanup-tmp dir))

    (test-case "verifier approval persists DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir rec 0 #t))
      (check-eq? (wave-status* rec 0) 'done "approved verifier persists 'done")
      (check-eq? (completion-result-status result) 'done)
      (check-not-false (completion-result-event-id result) "completion event ID is set")
      (cleanup-tmp dir))

    (test-case "approval cannot complete a wave that is not VERIFYING"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (persist-campaign! dir rec)
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id "missing"
                            #:expected-fence-token 0))
      (check-eq? (completion-result-status result) 'invalid-state)
      (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0) 'pending)
      (cleanup-tmp dir))

    (test-case "stale attempt cannot overwrite newer durable VERIFYING state"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define stale (load-campaign-record dir (campaign-plan-id rec)))
      (begin
        (set-campaign-fence-token! rec 2)
        (begin-attempt! rec 0 2))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir stale 0 #t))
      (check-eq? (completion-result-status result) 'stale-attempt)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'verifying)
      (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt (wave* durable 0)))
                    2)
      (cleanup-tmp dir))

    (test-case "second completion of already-DONE wave returns 'already-done"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (complete-current! dir rec 0 #t)
      (define r2 (complete-current! dir rec 0 #t))
      (check-eq? (completion-result-status r2) 'already-done)
      (cleanup-tmp dir))))

(define skip-suite
  (test-suite "/skip lifecycle"

    (test-case "/skip commits DEFERRED durably"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result (skip-wave! dir rec 0))
      (check-eq? (completion-result-status result) 'deferred)
      (check-eq? (wave-status* rec 0) 'deferred)
      (check-equal? (select-next-actionable-wave rec) 1 "deferred wave is not selected; next wave is")
      ;; Persisted
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* loaded 0) 'deferred)
      (cleanup-tmp dir))

    (test-case "/skip on already-done returns 'already-done"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (complete-current! dir rec 0 #t)
      (define result (skip-wave! dir rec 0))
      (check-eq? (completion-result-status result) 'already-done)
      (cleanup-tmp dir))))

(define outbox-suite
  (test-suite "durable outbox deduplication"

    (test-case "exactly one completion event per wave/attempt"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define r1 (complete-current! dir rec 0 #t))
      (check-not-false (completion-result-event-id r1))
      (check-equal? (count-completion-events dir rec) 1 "exactly one event in outbox"))))

(define no-heuristic-suite
  (test-suite "no heuristic implies completion (GC-5)"

    (test-case "wave doc existence never implies DONE"
      (define dir (make-tmp-campaign-dir 2))
      (make-directory* (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "waves" "W0-wave.md")
                             (lambda (out) (display "## Done!\nFully implemented.\n" out))
                             #:exists 'truncate)
      (define rec (load-or-migrate dir))
      (check-false (eq? (wave-status* rec 0) 'done) "doc existence does not infer completion"))))

;; ============================================================
;; W3 (verification-truth): durable failure reason on completion
;; ============================================================

(define failure-reason-suite
  (test-suite "durable failure reason on completion"
    (test-case "verifier rejection persists the verifier message durably"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #f
                            #:verifier-message "no wave target files changed: src/foo.rkt"
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-equal? (wave-failure-reason (wave* loaded 0))
                    "no wave target files changed: src/foo.rkt")
      (check-equal? (attempt-failure-reason (campaign-wave-current-attempt (wave* loaded 0)))
                    "no wave target files changed: src/foo.rkt")
      (cleanup-tmp dir))

    (test-case "blank verifier message gets an honest named fallback reason"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #f
                            #:verifier-message ""
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (define reason (wave-failure-reason (wave* loaded 0)))
      (check-true (and (string? reason)
                       (positive? (string-length reason))
                       (string-contains? reason "verifier rejected"))
                  (format "blank verdicts never persist as blank: ~s" reason))
      (cleanup-tmp dir))

    (test-case "release-gate failure persists the release reason durably"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:release-check (lambda () "no GitHub Release for v1.2.3")))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-true (string-contains? (wave-failure-reason (wave* loaded 0)) "release not verified"))
      (cleanup-tmp dir))

    (test-case "approval clears a previously stamped failure reason"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (stamp-wave-failure! (wave* rec 0) "stale prior failure")
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'done)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-equal? (wave-failure-reason (wave* loaded 0))
                    ""
                    "a completed wave carries no failure reason")
      (check-false (attempt-failure-reason (campaign-wave-current-attempt (wave* loaded 0))))
      (cleanup-tmp dir))))

;; ============================================================
;; Delivery-gated completion (v1.00.31 W3, register F9 + F10)
;; ============================================================

(define delivery-gate-suite
  (test-suite "delivery-gated completion"

    (test-case "F9: completing with require-delivered while delivery is pending is refused"
      ;; The observed v1.00.30 W4 incident state: [DONE]/Status: DONE while the
      ;; delivery journal is not delivered. The completion path must refuse
      ;; with the typed delivery-pending-cannot-complete result and leave all
      ;; durable surfaces untouched.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (seed-status! dir 0)
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:delivery-proof 'require-delivered))
      (check-eq? (completion-result-status result) 'delivery-pending-cannot-complete)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'verifying "no premature DONE is persisted")
      (check-equal? (count-completion-events dir durable) 0 "no leading completion event")
      (check-true (string-contains? (call-with-input-file (build-path dir ".planning" "PLAN.md")
                                                          port->string)
                                    "[Inbox] W0")
                  "plan index bracket stays Inbox on refusal")
      (check-true
       (string-contains? (call-with-input-file (build-path dir ".planning" "waves" "W0-wave.md")
                                               port->string)
                         "Status: Inbox")
       "wave doc header stays Inbox on refusal")
      (check-false (delivery-handoff-status dir (campaign-plan-id rec) 0))
      (cleanup-tmp dir))

    (test-case "F9: an authoritative delivered journal authorizes require-delivered completion"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define wave (wave* rec 0))
      (persist-delivery-handoff! dir pid wave "operator handoff")
      (reconcile-delivered-handoff! dir pid 0 (make-string 40 #\a))
      (check-eq? (delivery-handoff-status dir pid 0) 'delivered)
      (define attempt (campaign-wave-current-attempt wave))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:delivery-proof 'require-delivered))
      (check-eq? (completion-result-status result) 'done)
      (define durable (load-campaign-record dir pid))
      (check-eq? (wave-status* durable 0) 'done)
      ;; Lockstep: plan index bracket, wave doc header and journal agree.
      (check-true (string-contains? (call-with-input-file (build-path dir ".planning" "PLAN.md")
                                                          port->string)
                                    "[DONE] W0"))
      (check-true (string-contains?
                   (call-with-input-file (build-path dir ".planning" "waves" "W0-wave.md")
                                         port->string)
                   "Status: DONE"))
      (check-eq? (delivery-handoff-status dir pid 0) 'delivered)
      (check-eq? (completion-outbox-invariant? dir durable) 'ok)
      (cleanup-tmp dir))

    (test-case "F9: default completion records the typed carry-forward handoff"
      ;; A normal completion (delivery still pending) persists the typed
      ;; carry-forward record atomically with the durable DONE, so the
      ;; v1.00.30 W4 incident state (DONE without any journal) is impossible.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'done)
      (check-eq? (delivery-handoff-status dir pid 0) 'delivery-pending)
      (define durable (load-campaign-record dir pid))
      (check-eq? (completion-outbox-invariant? dir durable) 'ok)
      (cleanup-tmp dir))

    (test-case "F10: rolling a done wave back leaves no leading completion event"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (try-complete-wave! dir
                          rec
                          0
                          #:verifier-approve? #t
                          #:expected-attempt-id (campaign-attempt-id attempt)
                          #:expected-fence-token (campaign-attempt-fence-token attempt))
      (define durable (load-campaign-record dir pid))
      (check-equal? (count-completion-events dir durable) 1)
      ;; Roll the done wave back to pending (the W4-era repair shape).
      (set-campaign-wave-status! (wave* durable 0) 'pending)
      (persist-campaign! dir durable)
      (define rolled (load-campaign-record dir pid))
      (check-eq? (completion-outbox-invariant? dir rolled)
                 'outbox-leads-record
                 "the derived outbox leads the rolled-back record")
      (check-eq? (reconcile-completion-outbox! dir rolled) 0 "nothing to append")
      (check-equal? (count-completion-events dir (load-campaign-record dir pid))
                    0
                    "the leading event was pruned")
      (check-eq? (completion-outbox-invariant? dir (load-campaign-record dir pid)) 'ok)
      (cleanup-tmp dir))

    (test-case "F10: reconcile drops an invented event for a never-done wave"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define fake-id (make-event-id pid 0 "attempt-fake"))
      ;; Forge a leading event through the same atomic writer the outbox uses.
      (define p (build-path dir ".planning" "campaigns" (string-append pid ".outbox.rktd")))
      (make-directory* (build-path dir ".planning" "campaigns"))
      (call-with-output-file p (lambda (out) (write (list fake-id) out)) #:exists 'truncate)
      (define durable (load-campaign-record dir pid))
      (check-eq? (completion-outbox-invariant? dir durable) 'outbox-leads-record)
      (check-eq? (reconcile-completion-outbox! dir durable) 0)
      (check-false
       (file-exists? (build-path dir ".planning" "campaigns" (string-append pid ".outbox.rktd")))
       "empty pruned outbox is removed")
      (cleanup-tmp dir))))

;; ============================================================
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-wave-completion (W1)"
    verifier-first-suite
    skip-suite
    outbox-suite
    no-heuristic-suite
    failure-reason-suite
    delivery-gate-suite))

(void (run-tests all-tests))
