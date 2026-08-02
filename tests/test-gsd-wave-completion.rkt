#lang racket/base

;; @speed fast  ;; @suite extensions

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
                  set-campaign-wave-status!
                  begin-attempt!
                  select-next-actionable-wave
                  persist-campaign!
                  load-campaign-record
                  migrate-campaign!)
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
  dir)

(define (load-or-migrate dir)
  (migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave-status* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (campaign-wave-status w)))

;; ============================================================
;; Test suites
;; ============================================================

(define verifier-first-suite
  (test-suite "verifier-first completion"

    (test-case "verifier rejection cannot persist DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin-attempt! rec 0 1)
      (define result (try-complete-wave! dir rec 0 #:verifier-approve? #f))
      (check-eq? (wave-status* rec 0) 'failed "rejected verifier marks wave 'failed, not 'done")
      (check-eq? (completion-result-status result) 'failed)
      ;; Verify it was persisted
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* loaded 0) 'failed)
      (cleanup-tmp dir))

    (test-case "verifier approval persists DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin-attempt! rec 0 1)
      (define result (try-complete-wave! dir rec 0 #:verifier-approve? #t))
      (check-eq? (wave-status* rec 0) 'done "approved verifier persists 'done")
      (check-eq? (completion-result-status result) 'done)
      (check-not-false (completion-result-event-id result) "completion event ID is set")
      (cleanup-tmp dir))

    (test-case "second completion of already-DONE wave returns 'already-done"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin-attempt! rec 0 1)
      (try-complete-wave! dir rec 0 #:verifier-approve? #t)
      (define r2 (try-complete-wave! dir rec 0 #:verifier-approve? #t))
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
      (begin-attempt! rec 0 1)
      (try-complete-wave! dir rec 0 #:verifier-approve? #t)
      (define result (skip-wave! dir rec 0))
      (check-eq? (completion-result-status result) 'already-done)
      (cleanup-tmp dir))))

(define outbox-suite
  (test-suite "durable outbox deduplication"

    (test-case "exactly one completion event per wave/attempt"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin-attempt! rec 0 1)
      (define r1 (try-complete-wave! dir rec 0 #:verifier-approve? #t))
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
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-wave-completion (W1)"
    verifier-first-suite
    skip-suite
    outbox-suite
    no-heuristic-suite))

(void (run-tests all-tests))
