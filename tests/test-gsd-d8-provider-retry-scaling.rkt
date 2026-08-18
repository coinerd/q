#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-d8-provider-retry-scaling.rkt — D8 regression tests
;;
;; Campaign 81f9be4b W3 attempt-4: 30/30 tool calls completed, then ONE
;; transient 120 s SSE read timeout → progressive-stall circuit break →
;; @boundary integration
;; turn error → wave-failed, attempt consumed. The executor inherited the
;; interactive provider-retry policy (max-retries 2, stall-breaker 2,
;; 300 s ceiling) against the then-1800 s wave budget (the current default is
;; now 3600 s, and the retry ceiling stays capped at 900 s).
;;
;; D8 fix (#9357), two parts:
;;   (A) campaign-aware retry scaling — execute-campaign-request! parameterizes
;;       current-provider-retry-max-retries/stall/ceiling to wave-scale values;
;;   (B) infra-meta-fix — a provider/network/SSE failure produces the
;;       'infra-failed outcome and run-campaign-wave resets the wave to
;;       pending WITHOUT consuming the attempt.

(require rackunit
         racket/file
         racket/string
         "../runtime/provider-retry.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  prompt-run-result->outcome
                  run-campaign-wave
                  campaign-result-status
                  campaign-result-message)
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-wave
                  make-campaign-record
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-current-attempt)
         (only-in "../extensions/gsd/campaign-repository.rkt"
                  persist-campaign!
                  load-campaign-record
                  load-or-migrate-campaign!)
         (only-in "../extensions/gsd/campaign-state.rkt" migrate-campaign!)
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  wave-execution-outcome-message)
         (only-in "../util/loop-result.rkt" make-loop-result loop-result-termination-reason)
         (only-in "../extensions/gsd/wave-status.rkt" STATUS-DONE STATUS-FAILED))

;; ============================================================
;; Fixtures
;; ============================================================

(define (make-campaign-base)
  (define dir (make-temporary-file "d8-campaign-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display "# Plan: D8 Test\n\n## Waves\n\n- [Inbox] W0: Wave 0 → waves/W0-wave.md\n" out))
   #:exists 'truncate)
  dir)

(define (load-or-migrate dir)
  (load-or-migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave-status* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (campaign-wave-status w)))

(define (wave-attempt-count* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (campaign-wave-attempt-count w)))

;; ============================================================
;; (A) provider-retry scaling parameters
;; ============================================================

(test-case "D8(A): retry parameters default to interactive values"
  (check-equal? (current-provider-retry-max-retries) 2)
  (check-equal? (current-provider-retry-stall-max-consecutive) 2)
  (check-false (current-provider-retry-ceiling-secs)))

(test-case "D8(A): parameters are adjustable via parameterize"
  (parameterize ([current-provider-retry-max-retries 5]
                 [current-provider-retry-stall-max-consecutive 4]
                 [current-provider-retry-ceiling-secs 600])
    (check-equal? (current-provider-retry-max-retries) 5)
    (check-equal? (current-provider-retry-stall-max-consecutive) 4)
    (check-equal? (current-provider-retry-ceiling-secs) 600))
  ;; Restored after parameterize exits
  (check-equal? (current-provider-retry-max-retries) 2))

;; ============================================================
;; (B) infra classification → 'infra-failed outcome
;; ============================================================

(test-case "D8(B): provider stream error → infra-failed outcome"
  (define loop-result
    (make-loop-result '()
                      'error
                      (hasheq 'errorType
                              '(provider "The API provider returned an error." '())
                              'error
                              "HTTP read timeout (120 seconds) waiting for SSE chunk")))
  (define outcome (prompt-run-result->outcome loop-result))
  (check-eq? (wave-execution-outcome-kind outcome) 'infra-failed)
  (check-true (string-contains? (wave-execution-outcome-message outcome) "wave preserved")))

(test-case "D8(B): network read timeout → infra-failed outcome"
  (define loop-result
    (make-loop-result '()
                      'error
                      (hasheq 'errorType
                              '(network "A network error occurred." '())
                              'error
                              "connection reset by peer")))
  (define outcome (prompt-run-result->outcome loop-result))
  (check-eq? (wave-execution-outcome-kind outcome) 'infra-failed))

(test-case "D8(B): retry-exhausted marker → infra-failed outcome"
  (define loop-result
    (make-loop-result
     '()
     'error
     (hasheq 'errorType #f 'error "stream stalled" 'retries-attempted 1 'total-retry-delay-ms 2000)))
  (define outcome (prompt-run-result->outcome loop-result))
  (check-eq? (wave-execution-outcome-kind outcome) 'infra-failed))

(test-case "D8(B): genuine agent failure stays 'failed"
  (define loop-result
    (make-loop-result '()
                      'error
                      (hasheq 'errorType
                              '(contract "A contract violation occurred." '())
                              'error
                              "extension-ctx-working-directory: contract violation")))
  (define outcome (prompt-run-result->outcome loop-result))
  (check-eq? (wave-execution-outcome-kind outcome) 'failed))

(test-case "D8(B): tool loop limit stays 'failed"
  (define loop-result
    (make-loop-result '() 'error (hasheq 'toolLoopLimit #t 'error "tool loop limit reached")))
  (define outcome (prompt-run-result->outcome loop-result))
  (check-eq? (wave-execution-outcome-kind outcome) 'failed))

;; ============================================================
;; (B) run-campaign-wave: infra-failed does NOT consume the attempt
;; ============================================================

(test-case "D8(B): infra-failed resets wave to pending, attempt preserved"
  (define dir (make-campaign-base))
  (dynamic-wind void
                (lambda ()
                  (define rec (load-or-migrate dir))
                  ;; Precondition: attempt count is 0, status pending
                  (check-eq? (wave-status* rec 0) 'pending)
                  (check-equal? (wave-attempt-count* rec 0) 0)
                  ;; Runner returns an infra-failed outcome directly.
                  (define result
                    (run-campaign-wave dir
                                       rec
                                       0
                                       #:runner (lambda (_)
                                                  (wave-execution-outcome 'infra-failed
                                                                          "provider/network failure"))
                                       #:verifier (lambda (_) #t)))
                  (check-eq? (campaign-result-status result) 'wave-cancelled)
                  (check-true (string-contains? (campaign-result-message result) "not consumed"))
                  ;; Durable record: wave back to pending, attempt count UNCHANGED (0).
                  (define after (load-or-migrate dir))
                  (check-eq? (wave-status* after 0) 'pending)
                  (check-equal? (wave-attempt-count* after 0) 0))
                (lambda () (cleanup-tmp dir))))

(test-case "D8(B): genuine failure still consumes an attempt"
  (define dir (make-campaign-base))
  (dynamic-wind void
                (lambda ()
                  (define rec (load-or-migrate dir))
                  (define result
                    (run-campaign-wave dir
                                       rec
                                       0
                                       #:runner (lambda (_)
                                                  (wave-execution-outcome 'failed "runner error"))
                                       #:verifier (lambda (_) #t)))
                  (check-eq? (campaign-result-status result) 'wave-failed)
                  (define after (load-or-migrate dir))
                  (check-eq? (wave-status* after 0) 'failed)
                  (check-equal? (wave-attempt-count* after 0) 1))
                (lambda () (cleanup-tmp dir))))
