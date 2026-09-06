#lang racket/base

;; @speed fast  ;; @suite tui
;; @boundary unit

;; tests/test-tui-verification-display.rkt
;; W6 (v0.99.5): TUI display tests for HITL verification escalation events.
;;
;; Tests that verification events (started, completed, escalated) produce
;; correct TUI state transitions: status messages and transcript entries.

(require racket/string
         rackunit
         rackunit/text-ui
         "../tui/state-events.rkt"
         "../tui/state-types.rkt"
         "../tui/render/status-line.rkt"
         "../util/message/protocol-types.rkt"
         (only-in "../tui/render/message-layout.rkt" styled-line->text))

;; ── Helpers ──

;; Construct a test event matching the TUI event struct format.
(define (make-test-event ev-type payload #:time [time 1000])
  (event 1 ev-type time "test-session" "turn-1" payload))

;; Build a verification-started payload hash (direct typed-event format).
(define (make-started-payload #:artifact-count [count 5])
  (hasheq 'type
          "gsd.verification.started"
          'timestamp
          1000
          'session-id
          "test-session"
          'turn-id
          "turn-1"
          'artifact-count
          count
          'wave-number
          #f
          'plan-path
          #f))

;; Build a verification-completed payload hash.
(define (make-completed-payload #:verdict [verdict "approve"]
                                #:reason [reason #f]
                                #:risk-level [risk-level "low"]
                                #:requires-human [requires-human #f])
  (hasheq 'type
          "gsd.verification.completed"
          'timestamp
          1000
          'session-id
          "test-session"
          'turn-id
          "turn-1"
          'verdict
          verdict
          'reason
          reason
          'risk-level
          risk-level
          'requires-human
          requires-human))

;; Build a verification-escalated payload hash.
(define (make-escalated-payload #:reason [reason "Uncertain"]
                                #:risk-level [risk-level "high"]
                                #:artifact-refs [refs '()])
  (hasheq 'type
          "gsd.verification.escalated"
          'timestamp
          1000
          'session-id
          "test-session"
          'turn-id
          "turn-1"
          'reason
          reason
          'risk-level
          risk-level
          'artifact-refs
          refs))

;; Build a GSD-wrapped payload (from ctx-emit-gsd-event!).
(define (make-wrapped-payload inner-hash)
  (hasheq 'event
          'gsd.verification.escalated
          'correlation-id
          "test-correlation"
          'timestamp
          1000
          'data
          inner-hash
          '__typed
          #t))

;; ── Test Suite ──

(define suite
  (test-suite "TUI Verification Display (W6 v0.99.5)"

    ;; ── Registration ──

    (test-case "verification event handlers are registered"
      (check-true (event-reducer-registered? "gsd.verification.started"))
      (check-true (event-reducer-registered? "gsd.verification.completed"))
      (check-true (event-reducer-registered? "gsd.verification.escalated")))

    ;; ── Verification Started ──

    (test-case "verification-started sets status message"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (make-started-payload #:artifact-count 5))))
      (check-equal? (ui-state-status-message st1) "Verifying 5 artifacts..."))

    (test-case "verification-started with 0 artifacts"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (make-started-payload #:artifact-count 0))))
      (check-equal? (ui-state-status-message st1) "Verifying 0 artifacts..."))

    ;; ── Verification Completed: Approve ──

    (test-case "verification-completed (approve) clears status message"
      (define st0 (set-status-message (initial-ui-state) "Verifying..."))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.completed"
                                               (make-completed-payload #:verdict "approve"))))
      (check-false (ui-state-status-message st1)))

    ;; ── Verification Completed: Reject ──

    (test-case "verification-completed (reject) adds transcript entry"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.completed"
                                               (make-completed-payload #:verdict "reject"
                                                                       #:reason "Tests fail"))))
      (define transcript (ui-state-transcript st1))
      (check-true (pair? transcript) "transcript has at least one entry")
      (define entry (car transcript))
      (check-equal? (transcript-entry-kind entry) 'system)
      (check-true (string-contains? (transcript-entry-text entry) "REJECTED"))
      (check-true (string-contains? (transcript-entry-text entry) "Tests fail")))

    (test-case "verification-completed (reject) without reason"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.completed"
                                               (make-completed-payload #:verdict "reject"
                                                                       #:reason #f))))
      (define transcript (ui-state-transcript st1))
      (define entry (car transcript))
      (check-true (string-contains? (transcript-entry-text entry) "REJECTED")))

    ;; ── Verification Completed: Symbol verdicts ──

    (test-case "verification-completed handles symbol verdicts"
      (define st0 (set-status-message (initial-ui-state) "Verifying..."))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.completed"
                                               (make-completed-payload #:verdict 'approve))))
      (check-false (ui-state-status-message st1)))

    ;; ── Verification Escalated ──

    (test-case "verification-escalated sets status + transcript entry"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.escalated"
                                               (make-escalated-payload #:reason "Uncertain"
                                                                       #:risk-level "high"))))
      (check-equal? (ui-state-status-message st1) "Verification requires approval")
      (define transcript (ui-state-transcript st1))
      (check-true (pair? transcript) "transcript has entry")
      (define entry (car transcript))
      (check-equal? (transcript-entry-kind entry) 'system)
      (check-true (string-contains? (transcript-entry-text entry) "Uncertain"))
      (check-true (string-contains? (transcript-entry-text entry) "high")))

    (test-case "verification-escalated transcript entry has metadata"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.escalated"
                                               (make-escalated-payload))))
      (define entry (car (ui-state-transcript st1)))
      (define meta (transcript-entry-meta entry))
      (check-equal? (hash-ref meta 'verification) #t)
      (check-equal? (hash-ref meta 'escalation) #t))

    ;; ── Multiple Escalations ──

    (test-case "multiple escalations produce multiple transcript entries"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.escalated"
                                               (make-escalated-payload #:reason "First"
                                                                       #:risk-level "high"))))
      (define st2
        (apply-event-to-state st1
                              (make-test-event "gsd.verification.escalated"
                                               (make-escalated-payload #:reason "Second"
                                                                       #:risk-level "high"))))
      (define transcript (ui-state-transcript st2))
      (check-equal? (length transcript) 2)
      (check-true (string-contains? (transcript-entry-text (car transcript)) "Second"))
      (check-true (string-contains? (transcript-entry-text (cadr transcript)) "First")))

    ;; ── Missing Fields Graceful Handling ──

    (test-case "verification-escalated with missing reason uses default"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state
         st0
         (make-test-event
          "gsd.verification.escalated"
          (hasheq 'type "gsd.verification.escalated" 'timestamp 1000 'session-id "s" 'turn-id "t"))))
      (define entry (car (ui-state-transcript st1)))
      (check-true (string-contains? (transcript-entry-text entry) "unknown")))

    (test-case "verification-started with missing artifact-count uses default 0"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (hasheq 'type "gsd.verification.started"))))
      (check-equal? (ui-state-status-message st1) "Verifying 0 artifacts..."))

    ;; ── GSD-Wrapped Payload Format ──

    (test-case "verification-escalated handles GSD-wrapped payload"
      (define st0 (initial-ui-state))
      (define inner (make-escalated-payload #:reason "Wrapped reason" #:risk-level "medium"))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.escalated"
                                               (make-wrapped-payload inner))))
      (define entry (car (ui-state-transcript st1)))
      (check-true (string-contains? (transcript-entry-text entry) "Wrapped reason"))
      (check-true (string-contains? (transcript-entry-text entry) "medium")))

    ;; ── BUG-0058: coordinator-owned verification progress ──

    (test-case "coordinator started event renders wave-scoped status and stores progress"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (hasheq 'wave
                                                       4
                                                       'command
                                                       "racket scripts/run-tests.rkt --suite fast"
                                                       'timeout-sec
                                                       7200
                                                       'log-path
                                                       "/var/tmp/gsd-verification-vj-1.log"
                                                       'attached?
                                                       #f
                                                       'start-ms
                                                       1000.0))))
      (define status (ui-state-status-message st1))
      (check-true (string-contains? status "W4 verifying") status)
      (check-true (string-contains? status "budget 7200s") status)
      (check-true (string-contains? status "/var/tmp/gsd-verification-vj-1.log") status)
      (define progress (ui-state-verification-progress st1))
      (check-true (hash? progress) "progress stored while verification is active")
      (check-equal? (hash-ref progress 'wave) 4)
      (check-equal? (hash-ref progress 'started-ms) 1000.0))

    (test-case "coordinator completed (approve) clears status and progress"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (hasheq 'wave
                                                       4
                                                       'command
                                                       "true"
                                                       'timeout-sec
                                                       60
                                                       'log-path
                                                       "/var/tmp/v.log"
                                                       'start-ms
                                                       1000.0))))
      (define st2
        (apply-event-to-state st1
                              (make-test-event "gsd.verification.completed"
                                               (hasheq 'wave
                                                       4
                                                       'state
                                                       'completed
                                                       'exit-code
                                                       0
                                                       'approved?
                                                       #t
                                                       'verdict
                                                       "approve"
                                                       'elapsed-sec
                                                       12.5
                                                       'log-path
                                                       "/var/tmp/v.log"))))
      (check-false (ui-state-status-message st2))
      (check-false (ui-state-verification-progress st2)))

    (test-case "coordinator completed (reject) records elapsed + log durably"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (hasheq 'wave
                                                       0
                                                       'command
                                                       "false"
                                                       'timeout-sec
                                                       60
                                                       'log-path
                                                       "/var/tmp/v.log"
                                                       'start-ms
                                                       1000.0))))
      (define st2
        (apply-event-to-state st1
                              (make-test-event "gsd.verification.completed"
                                               (hasheq 'wave
                                                       0
                                                       'state
                                                       'failed
                                                       'exit-code
                                                       1
                                                       'approved?
                                                       #f
                                                       'verdict
                                                       "reject"
                                                       'reason
                                                       "verify state=failed exit=1"
                                                       'elapsed-sec
                                                       252.0
                                                       'log-path
                                                       "/var/tmp/v.log"))))
      (check-false (ui-state-verification-progress st2))
      (define entry (car (ui-state-transcript st2)))
      (define text (transcript-entry-text entry))
      (check-true (string-contains? text "REJECTED") text)
      (check-true (string-contains? text "verify state=failed exit=1") text)
      (check-true (string-contains? text "4m 12s") text)
      (check-true (string-contains? text "/var/tmp/v.log") text))

    (test-case "status bar renders live elapsed while verification is active"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (hasheq 'wave
                                                       4
                                                       'command
                                                       "raco test"
                                                       'timeout-sec
                                                       7200
                                                       'log-path
                                                       "/var/tmp/v.log"
                                                       'start-ms
                                                       (- (current-inexact-milliseconds) 65000.0)))))
      (define line (render-status-bar st1 120))
      (define text (styled-line->text line))
      (check-true (string-contains? text "W4 verifying") text)
      (check-true (regexp-match? #rx"1m [0-9]+s elapsed" text) text))

    (test-case "status bar renders no elapsed when verification is inactive"
      (define line (render-status-bar (initial-ui-state) 120))
      (check-false (string-contains? (styled-line->text line) "elapsed")))

    ;; ── Full Lifecycle ──

    (test-case "full verification lifecycle: started → completed (approve)"
      (define st0 (initial-ui-state))
      ;; Started
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (make-started-payload #:artifact-count 3))))
      (check-equal? (ui-state-status-message st1) "Verifying 3 artifacts...")
      ;; Completed (approve)
      (define st2
        (apply-event-to-state st1
                              (make-test-event "gsd.verification.completed"
                                               (make-completed-payload #:verdict "approve"))))
      (check-false (ui-state-status-message st2)))

    (test-case "full verification lifecycle: started → escalated"
      (define st0 (initial-ui-state))
      ;; Started
      (define st1
        (apply-event-to-state st0
                              (make-test-event "gsd.verification.started"
                                               (make-started-payload #:artifact-count 2))))
      (check-equal? (ui-state-status-message st1) "Verifying 2 artifacts...")
      ;; Escalated
      (define st2
        (apply-event-to-state st1
                              (make-test-event "gsd.verification.escalated"
                                               (make-escalated-payload #:reason "Need human review"
                                                                       #:risk-level "high"))))
      (check-equal? (ui-state-status-message st2) "Verification requires approval")
      (define transcript (ui-state-transcript st2))
      (check-true (pair? transcript) "escalation added transcript entry"))

    ;; ── BUG-0062: live GSD event shape ──
    ;;
    ;; The coordinator emits via emit-gsd-event! with a SYMBOL event name and
    ;; a wrapped payload ({event, correlation-id, timestamp, data}). These
    ;; tests construct exactly that shape — the previous string-ev tests
    ;; passed while every live gsd.* event was silently dropped at dispatch.

    (test-case "live shape: symbol-ev started (wrapped payload) renders status + progress"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state
         st0
         (make-test-event 'gsd.verification.started
                          (hasheq 'event
                                  'gsd.verification.started
                                  'correlation-id
                                  #f
                                  'timestamp
                                  1000.0
                                  'data
                                  (hasheq 'wave
                                          0
                                          'command
                                          "racket scripts/run-tests.rkt --suite fast"
                                          'timeout-sec
                                          1800
                                          'log-path
                                          "/var/tmp/gsd-verification-vj-1.log"
                                          'attached?
                                          #f
                                          'start-ms
                                          1788685499468.0)))))
      (check-equal? (ui-state-status-message st1)
                    "W0 verifying — budget 1800s · log /var/tmp/gsd-verification-vj-1.log")
      (define progress (ui-state-verification-progress st1))
      (check-true (hash? progress) "verification-progress must be set")
      (check-equal? (hash-ref progress 'wave #f) 0))

    (test-case "live shape: symbol-ev completed approve clears status + adds durable entry"
      (define st0 (set-status-message (initial-ui-state) "W0 verifying — budget 1800s"))
      (define st1
        (apply-event-to-state
         st0
         (make-test-event 'gsd.verification.completed
                          (hasheq 'event
                                  'gsd.verification.completed
                                  'correlation-id
                                  #f
                                  'timestamp
                                  1000.0
                                  'data
                                  (hasheq 'wave
                                          0
                                          'command
                                          "true"
                                          'state
                                          'completed
                                          'exit-code
                                          0
                                          'approved?
                                          #t
                                          'verdict
                                          "approve"
                                          'reason
                                          #f
                                          'elapsed-sec
                                          2711.5
                                          'log-path
                                          "/var/tmp/gsd-verification-vj-1.log")))))
      (check-false (ui-state-status-message st1) "status cleared on approval")
      (define transcript (ui-state-transcript st1))
      (check-true (pair? transcript) "approval must leave a durable transcript entry")
      (define entry (car transcript))
      (check-equal? (transcript-entry-kind entry) 'system)
      (check-true (string-contains? (transcript-entry-text entry) "approved"))
      (check-true (string-contains? (transcript-entry-text entry) "45m 11s"))
      (check-true (string-contains? (transcript-entry-text entry)
                                    "/var/tmp/gsd-verification-vj-1.log")))

    (test-case "live shape: symbol-ev completed reject adds REJECTED entry"
      (define st0 (initial-ui-state))
      (define st1
        (apply-event-to-state
         st0
         (make-test-event 'gsd.verification.completed
                          (hasheq 'event
                                  'gsd.verification.completed
                                  'correlation-id
                                  #f
                                  'timestamp
                                  1000.0
                                  'data
                                  (hasheq 'wave
                                          0
                                          'command
                                          "false"
                                          'state
                                          'completed
                                          'exit-code
                                          1
                                          'approved?
                                          #f
                                          'verdict
                                          "reject"
                                          'reason
                                          "verify state=completed exit=1"
                                          'elapsed-sec
                                          12.0
                                          'log-path
                                          "/var/tmp/gsd-verification-vj-2.log")))))
      (define transcript (ui-state-transcript st1))
      (check-true (pair? transcript) "rejection must add a transcript entry")
      (define entry (car transcript))
      (check-true (string-contains? (transcript-entry-text entry) "REJECTED"))
      (check-true (string-contains? (transcript-entry-text entry) "verify state=completed exit=1")))))

(run-tests suite)
