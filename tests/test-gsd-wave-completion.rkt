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
         (only-in "../extensions/gsd/wave-completion.rkt"
                  try-complete-wave!
                  skip-wave!
                  completion-result-status
                  completion-result-event-id
                  count-completion-events))

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
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-wave-completion (W1)"
    verifier-first-suite
    skip-suite
    outbox-suite
    no-heuristic-suite
    failure-reason-suite))

(void (run-tests all-tests))
