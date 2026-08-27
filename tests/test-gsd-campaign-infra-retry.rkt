#lang racket

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-gsd-campaign-infra-retry.rkt — BUG-0024 W3 regression tests
;;
;; On 'infra-failed the wave used to be reset to pending but the whole
;; campaign returned wave-cancelled ("re-run /go") — terminal. Observed 5
;; times during the v1.00.17 campaign; each stop required a manual /retry
;; and the fresh executor session re-explored from scratch (~12 duplicated
;; tool calls) because no attempt context survives a session restart.
;;
;; W3 fix, three parts:
;;   (1) campaign-level infra retry — on infra-failed the SAME wave is
;;       re-attempted automatically with exponential backoff (30s/60s/120s)
;;       bounded by current-gsd-campaign-infra-retries (default 3, settings
;;       key gsd.campaign-infra-retries). The attempt is NOT consumed (D8
;;       semantics). Each retry emits gsd.campaign.infra-retry. Exhaustion
;;       stops the campaign with an aggregated message listing every
;;       failure timestamp.
;;   (2) attempt-context hand-off — the durable wave record carries an
;;       attempt-context field (capped ~2 KB); the automatic re-attempt
;;       receives it through current-gsd-wave-failure-context (existing
;;       #9515 prompt plumbing, no parallel state); success clears it.
;;   (3) these tests: auto-resume, bounded exhaustion, context hand-off,
;;       event observability, block formatting.
;;
;; BUG-0037 W1 (this release): watchdog death during a wave with ZERO file
;; mutations (exploration phase) is retryable infrastructure — the same
;; bounded auto-resume path as provider/network failures, with PRIOR
;; ATTEMPT CONTEXT and gsd.campaign.infra-retry events. Test (6) below
;; pins the campaign-level contract: a stall kill must never surface as
;; a terminal campaign stop.

(require rackunit
         racket/file
         racket/string
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign-wave
                  campaign-result-status
                  campaign-result-message
                  stall-hard-failure-message)
         (only-in "../extensions/gsd/campaign-state.rkt"
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-attempt-context)
         (only-in "../extensions/gsd/campaign-repository.rkt" load-or-migrate-campaign!)
         (only-in "../extensions/gsd/wave-runner-port.rkt" wave-execution-outcome)
         (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-campaign-infra-retries
                  current-gsd-campaign-infra-retry-delay
                  current-gsd-wave-failure-context)
         (only-in "../extensions/gsd/prompts.rkt" wave-attempt-context-block)
         (only-in "../extensions/gsd/events.rkt"
                  make-event-collector
                  collector-events
                  set-gsd-event-bus!))

;; ============================================================
;; Fixtures (same shape as test-gsd-d8-provider-retry-scaling.rkt)
;; ============================================================

(define (make-campaign-base)
  (define dir (make-temporary-file "w3-infra-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display "# Plan: W3 Infra Retry Test\n\n## Waves\n\n- [Inbox] W0: Wave 0 → waves/W0-wave.md\n"
              out))
   #:exists 'truncate)
  dir)

(define (load-or-migrate dir)
  (load-or-migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave-field* rec idx acc)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (acc w)))

(define (wave-status* rec idx)
  (wave-field* rec idx campaign-wave-status))
(define (wave-attempt-count* rec idx)
  (wave-field* rec idx campaign-wave-attempt-count))
(define (wave-attempt-context* rec idx)
  (wave-field* rec idx campaign-wave-attempt-context))

;; Deterministic test policy: allow N automatic retries, zero delay.
(define (pin-infra-policy retries)
  (list (list current-gsd-campaign-infra-retries retries)))

;; ============================================================
;; (1) Transient infra failure auto-resumes without manual input
;; ============================================================

(test-case "W3: transient infra failure auto-resumes and completes the wave"
  (define dir (make-campaign-base))
  (dynamic-wind
   void
   (lambda ()
     (define rec (load-or-migrate dir))
     (define runs (box 0))
     (define result
       (parameterize ([current-gsd-campaign-infra-retries 3]
                      [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
         (run-campaign-wave dir
                            rec
                            0
                            #:runner (lambda (idx)
                                       (set-box! runs (add1 (unbox runs)))
                                       (if (= (unbox runs) 1)
                                           (wave-execution-outcome
                                            'infra-failed
                                            "HTTP read timeout (120 seconds) waiting for SSE chunk")
                                           'ok))
                            #:verifier (lambda (_) #t))))
     (check-eq? (campaign-result-status result) 'wave-done)
     ;; 1 infra run + 1 automatic re-attempt = 2 executor runs,
     ;; with NO manual /retry in between.
     (check-equal? (unbox runs) 2)
     (define after (load-or-migrate dir))
     (check-eq? (wave-status* after 0) 'done)
     ;; Infra failure did not consume the attempt; the successful
     ;; re-attempt is the only one recorded.
     (check-equal? (wave-attempt-count* after 0) 1)
     ;; Success clears the hand-off context.
     (check-equal? (wave-attempt-context* after 0) ""))
   (lambda () (cleanup-tmp dir))))

;; ============================================================
;; (2) Bound exhaustion stops the campaign with an aggregated message
;; ============================================================

(test-case "W3: infra retries are bounded — exhaustion stops with all failure timestamps"
  (define dir (make-campaign-base))
  (dynamic-wind
   void
   (lambda ()
     (define rec (load-or-migrate dir))
     (define runs (box 0))
     (define result
       (parameterize ([current-gsd-campaign-infra-retries 2]
                      [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
         (run-campaign-wave dir
                            rec
                            0
                            #:runner (lambda (idx)
                                       (set-box! runs (add1 (unbox runs)))
                                       (wave-execution-outcome 'infra-failed
                                                               "connection reset by peer"))
                            #:verifier (lambda (_) #t))))
     (check-eq? (campaign-result-status result) 'wave-cancelled)
     ;; 1 initial run + 2 automatic retries = 3 executor runs.
     (check-equal? (unbox runs) 3)
     ;; Aggregated message: the bound, plus every failure timestamp.
     ;; Attempt numbering is 1-based (attempt N = the Nth executor
     ;; run of this wave).
     (check-true (string-contains? (campaign-result-message result) "after 2 automatic retries"))
     (check-true (string-contains? (campaign-result-message result) "Failures:"))
     (check-true (string-contains? (campaign-result-message result) "attempt 1 at"))
     ;; Fail-closed but re-attemptable: pending, attempt NOT
     ;; consumed, context preserved for the next /go.
     (define after (load-or-migrate dir))
     (check-eq? (wave-status* after 0) 'pending)
     (check-equal? (wave-attempt-count* after 0) 0)
     (check-true (positive? (string-length (wave-attempt-context* after 0)))
                 "durable attempt-context must survive exhaustion"))
   (lambda () (cleanup-tmp dir))))

(test-case "W3: retries=0 keeps the deterministic fail-closed stop (D8 back-compat)"
  (define dir (make-campaign-base))
  (dynamic-wind void
                (lambda ()
                  (define rec (load-or-migrate dir))
                  (define runs (box 0))
                  (define result
                    (parameterize ([current-gsd-campaign-infra-retries 0]
                                   [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
                      (run-campaign-wave
                       dir
                       rec
                       0
                       #:runner (lambda (idx)
                                  (set-box! runs (add1 (unbox runs)))
                                  (wave-execution-outcome 'infra-failed "provider/network failure"))
                       #:verifier (lambda (_) #t))))
                  (check-eq? (campaign-result-status result) 'wave-cancelled)
                  (check-equal? (unbox runs) 1 "zero bound = exactly one run, immediate stop")
                  (define after (load-or-migrate dir))
                  (check-eq? (wave-status* after 0) 'pending)
                  (check-equal? (wave-attempt-count* after 0) 0))
                (lambda () (cleanup-tmp dir))))

;; ============================================================
;; (3) Attempt-context hand-off reaches the re-attempted prompt layer
;; ============================================================

(test-case "W3: automatic re-attempt receives the prior-attempt context block"
  (define dir (make-campaign-base))
  (dynamic-wind
   void
   (lambda ()
     (define rec (load-or-migrate dir))
     (define runs (box 0))
     (define contexts '())
     (define result
       (parameterize ([current-gsd-campaign-infra-retries 3]
                      [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
         (run-campaign-wave
          dir
          rec
          0
          #:runner (lambda (idx)
                     (set-box! runs (add1 (unbox runs)))
                     ;; Capture what the prompt layer
                     ;; sees on each executor session:
                     ;; current-gsd-wave-failure-context
                     ;; is exactly what single-wave
                     ;; prompts render (#9515 plumbing).
                     (set! contexts (append contexts (list (current-gsd-wave-failure-context))))
                     (if (= (unbox runs) 1)
                         (wave-execution-outcome 'infra-failed "connection reset by peer mid-wave")
                         'ok))
          #:verifier (lambda (_) #t))))
     (check-eq? (campaign-result-status result) 'wave-done)
     (check-equal? (length contexts) 2)
     ;; First attempt: no prior-attempt context.
     (check-false (and (string? (car contexts))
                       (string-contains? (car contexts) "PRIOR ATTEMPT CONTEXT")))
     ;; Automatic re-attempt: the block is present and carries the
     ;; prior failure's error text.
     (check-true (string? (cadr contexts)) "re-attempt must see a rendered context block")
     (check-true (string-contains? (cadr contexts) "PRIOR ATTEMPT CONTEXT"))
     (check-true (string-contains? (cadr contexts) "connection reset by peer"))
     ;; The block survives a session restart: durable record held it
     ;; before the re-attempt consumed it on success.
     (define after (load-or-migrate dir))
     (check-equal? (wave-attempt-context* after 0) ""))
   (lambda () (cleanup-tmp dir))))

;; ============================================================
;; (4) Every automatic retry is observable on the event bus
;; ============================================================

(test-case "W3: each automatic infra retry emits gsd.campaign.infra-retry"
  (define dir (make-campaign-base))
  (dynamic-wind
   void
   (lambda ()
     (define rec (load-or-migrate dir))
     ;; The event bus is a plain procedure (see events.rkt;
     ;; default void). Collect events in a box with a variadic
     ;; lambda so the test does not depend on the exact
     ;; calling convention of emit-gsd-event!.
     (define events-box (box '()))
     (set-gsd-event-bus! (lambda args (set-box! events-box (cons args (unbox events-box)))))
     (parameterize ([current-gsd-campaign-infra-retries 2]
                    [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
       (run-campaign-wave
        dir
        rec
        0
        #:runner (lambda (idx) (wave-execution-outcome 'infra-failed "connection reset by peer"))
        #:verifier (lambda (_) #t)))
     (define infra-events
       (filter (lambda (e) (regexp-match #rx"infra-retry" (format "~a" e))) (unbox events-box)))
     ;; 2 automatic retries → exactly 2 events, none for the
     ;; initial run, none when the bound is exhausted.
     (check-equal? (length infra-events) 2)
     ;; Payload carries the wave index (W0) and the retry delay.
     (for ([e (in-list infra-events)])
       (check-true (and (regexp-match #rx"wave" (format "~a" e)) #t)
                   "event payload must name the wave")
       (check-true (and (regexp-match #rx"delay" (format "~a" e)) #t)
                   "event payload must name the delay")))
   (lambda ()
     (set-gsd-event-bus! void)
     (cleanup-tmp dir))))

;; ============================================================
;; (5) wave-attempt-context-block formatting (prompts.rkt unit)
;; ============================================================

(test-case "W3: wave-attempt-context-block renders only non-empty contexts"
  (check-false (wave-attempt-context-block #f))
  (check-false (wave-attempt-context-block ""))
  (define block (wave-attempt-context-block "wave 0 attempt 0 failed: connection reset"))
  (check-true (string? block))
  (check-true (string-contains? block "PRIOR ATTEMPT CONTEXT"))
  (check-true (string-contains? block "END PRIOR ATTEMPT CONTEXT"))
  (check-true (string-contains? block "connection reset"))
  ;; The block instructs resume, not restart — the hand-off's whole point.
  (check-true (string-contains? block "do NOT restart")))

;; ============================================================
;; (6) BUG-0037 W1: exploration-phase stall death is retryable
;; ============================================================

;; A watchdog kill arrives at run-campaign-wave as the classified
;; 'infra-failed outcome built from stall-hard-failure-message (both the
;; gsd-stall-exn handler and prompt-run-result->outcome's stall-prefix
;; clause produce this exact shape). This pins the CAMPAIGN contract:
;; the kill consumes no attempt, emits gsd.campaign.infra-retry, hands
;; the re-attempt the stall's PRIOR ATTEMPT CONTEXT, and the wave
;; completes — a stall kill must never surface as a terminal stop.
(test-case "BUG-0037: stall kill during zero-mutation exploration auto-resumes the campaign"
  (define dir (make-campaign-base))
  (dynamic-wind
   void
   (lambda ()
     (define rec (load-or-migrate dir))
     (define runs (box 0))
     (define contexts '())
     ;; The canonical watchdog kill message (exactly what the live
     ;; classification produces for a repeated-read livelock).
     (define stall-msg
       (stall-hard-failure-message 15
                                   15
                                   '("q/extensions/gsd/wave-executor.rkt")
                                   "read"
                                   '(read read grep)))
     (check-true (string-prefix? stall-msg "mutation-stall watchdog:")
                 "fixture must be the canonical stall kill message")
     (define events-box (box '()))
     (set-gsd-event-bus! (lambda args (set-box! events-box (cons args (unbox events-box)))))
     (define result
       (parameterize ([current-gsd-campaign-infra-retries 2]
                      [current-gsd-campaign-infra-retry-delay (lambda (_) 0)])
         (run-campaign-wave
          dir
          rec
          0
          #:runner (lambda (idx)
                     (set-box! runs (add1 (unbox runs)))
                     (set! contexts (append contexts (list (current-gsd-wave-failure-context))))
                     (if (= (unbox runs) 1)
                         (wave-execution-outcome 'infra-failed stall-msg)
                         'ok))
          #:verifier (lambda (_) #t))))
     ;; The campaign CONTINUED past the stall kill and completed the wave.
     (check-eq? (campaign-result-status result) 'wave-done)
     (check-equal? (unbox runs) 2 "1 stall-killed run + 1 automatic re-attempt")
     ;; Retry observability: exactly one gsd.campaign.infra-retry event.
     (define infra-events
       (filter (lambda (e) (regexp-match #rx"infra-retry" (format "~a" e))) (unbox events-box)))
     (check-equal? (length infra-events) 1)
     ;; The re-attempt resumed from the stall's PRIOR ATTEMPT CONTEXT —
     ;; not from a blank prompt re-exploring from zero.
     (check-true (string? (cadr contexts)) "re-attempt must see a rendered context block")
     (check-true (string-contains? (cadr contexts) "PRIOR ATTEMPT CONTEXT"))
     (check-true (string-contains? (cadr contexts) "mutation-stall watchdog"))
     (check-true (string-contains? (cadr contexts) "do NOT restart"))
     ;; Durable record: done, retryable death consumed no attempt,
     ;; success cleared the hand-off context.
     (define after (load-or-migrate dir))
     (check-eq? (wave-status* after 0) 'done)
     (check-equal? (wave-attempt-count* after 0) 1)
     (check-equal? (wave-attempt-context* after 0) ""))
   (lambda ()
     (set-gsd-event-bus! void)
     (cleanup-tmp dir))))
