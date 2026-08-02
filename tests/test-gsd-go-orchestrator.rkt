#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-go-orchestrator.rkt — W2: Single-Wave Campaign Coordinator
;;
;; TDD tests for:
;;   1. One runner call per wave, prompt isolation.
;;   2. No advancement on failure/cancellation.
;;   3. Verifier rejection prevents DONE.
;;   4. /go N assertion rejects non-earliest wave.
;;   5. Duplicate lease rejection.
;;   6. Campaign completes when all waves done.

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
                  set-campaign-wave-status!
                  begin-attempt!
                  select-next-actionable-wave
                  persist-campaign!
                  load-campaign-record
                  migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign-wave
                  run-campaign!
                  assert-go-n
                  campaign-result-status
                  campaign-result-completed-waves
                  acquire-lease
                  release-lease!
                  campaign-lease?))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tmp-campaign-dir n-waves)
  (define dir (make-temporary-file "go-orch-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Campaign\n\n## Waves\n\n" out)
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

(define single-wave-suite
  (test-suite "single-wave execution"

    (test-case "runner succeeds + verifier approves → DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result (run-campaign-wave dir rec 0))
      (check-eq? (campaign-result-status result) 'wave-done)
      (check-eq? (wave-status* rec 0) 'done)
      (cleanup-tmp dir))

    (test-case "verifier rejects → no DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result (run-campaign-wave dir rec 0 #:verifier (lambda (_) #f)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-false (eq? (wave-status* rec 0) 'done) "verifier rejection cannot persist DONE")
      (cleanup-tmp dir))

    (test-case "runner error → FAILED"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result (run-campaign-wave dir rec 0 #:runner (lambda (_) 'error)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'failed)
      (cleanup-tmp dir))

    (test-case "runner cancelled → INTERRUPTED"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result (run-campaign-wave dir rec 0 #:runner (lambda (_) 'cancelled)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))))

(define campaign-suite
  (test-suite "full campaign execution"

    (test-case "all waves succeed → campaign-complete"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result (run-campaign! dir rec))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (check-equal? (campaign-result-completed-waves result) '(0 1 2))
      (cleanup-tmp dir))

    (test-case "failure stops campaign (no advancement)"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define call-count 0)
      (define result
        (run-campaign! dir
                       rec
                       #:runner (lambda (idx)
                                  (set! call-count (add1 call-count))
                                  (if (= idx 1) 'error 'ok))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-equal? call-count 2 "runner called for W0 and W1 only")
      (check-eq? (wave-status* rec 0) 'done)
      (check-eq? (wave-status* rec 1) 'failed)
      (check-false (eq? (wave-status* rec 2) 'done) "W2 must not advance past failed W1")
      (cleanup-tmp dir))

    (test-case "verifier rejection stops campaign"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result (run-campaign! dir rec #:verifier (lambda (idx) (not (= idx 1)))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'done)
      (check-false (eq? (wave-status* rec 1) 'done))
      (cleanup-tmp dir))))

(define lease-suite
  (test-suite "campaign lease (D5)"

    (test-case "duplicate lease is rejected"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define lease1 (acquire-lease dir (campaign-plan-id rec)))
      (check-true (campaign-lease? lease1))
      (define lease2 (acquire-lease dir (campaign-plan-id rec)))
      (check-false lease2 "second acquire returns #f")
      (release-lease! lease1)
      ;; After release, can re-acquire
      (define lease3 (acquire-lease dir (campaign-plan-id rec)))
      (check-true (campaign-lease? lease3))
      (release-lease! lease3)
      (cleanup-tmp dir))))

(define go-n-suite
  (test-suite "/go N assertion (D8)"

    (test-case "/go 0 when W0 actionable → allowed"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (check-true (assert-go-n rec 0))
      (cleanup-tmp dir))

    (test-case "/go 2 when W0 actionable → rejected"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (check-false (assert-go-n rec 2) "cannot bypass W0")
      (cleanup-tmp dir))

    (test-case "/go 1 when W0 done → allowed"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
      (check-true (assert-go-n rec 1))
      (cleanup-tmp dir))))

;; ============================================================
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-go-orchestrator (W2)"
    single-wave-suite
    campaign-suite
    lease-suite
    go-n-suite))

(void (run-tests all-tests))
