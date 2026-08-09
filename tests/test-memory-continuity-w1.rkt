#lang racket/base

;; test-memory-continuity-w1.rkt — Tests for memory continuity & looping prevention
;; Plan: .planning/PLAN-v0.96.13-MEMORY-CONTINUITY-LOOPING-PREVENTION.md

;; @speed fast
(require rackunit
         racket/string
         (only-in "../util/message/message.rkt"
                  make-message
                  message
                  message?
                  message-id
                  message-content
                  message-role)
         (only-in "../util/content/content-parts.rkt"
                  text-part
                  text-part?
                  text-part-text
                  make-text-part)
         (only-in "../runtime/context-assembly/memory-builder.rkt"
                  observe-memory-for-context
                  inject-memory-for-context)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" extract-recent-text)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  warnings->actions
                  rollback-action?
                  rollback-action-type
                  rollback-action-severity
                  current-rollback-state
                  make-default-rollback-state
                  rollback-state
                  rollback-warning-count
                  record-rollback-warning!
                  reset-rollback-warning-count!
                  escalation-threshold)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  build-state-awareness-preamble
                  check-rollback-triggers)
         (only-in "../runtime/iteration/step-executor.rkt"
                  current-reflection-prompt-enabled
                  REFLECTION-THRESHOLD-CHARS)
         (only-in "../runtime/working-set.rkt" ws-entry ws-entry->text ws-entry?))

;; ══════════════════════════════════════════════════════════════════
;; W1: Context-aware memory retrieval
;; ══════════════════════════════════════════════════════════════════

(test-case "W1.3: extract-recent-text returns text from assistant messages"
  (define msgs
    (list (make-message "m1" #f 'user 'user-msg (list (make-text-part "read file.rkt")) 100 (hasheq))
          (make-message "m2"
                        #f
                        'assistant
                        'assistant-msg
                        (list (make-text-part "Found the bug in auth-store"))
                        101
                        (hasheq))
          (make-message "m3" #f 'user 'user-msg (list (make-text-part "fix it")) 102 (hasheq))
          (make-message "m4"
                        #f
                        'assistant
                        'assistant-msg
                        (list (make-text-part "Fixed the auth bug by adding null check"))
                        103
                        (hasheq))))
  (define result (extract-recent-text msgs 2))
  (check-not-false result)
  (check-true (string-contains? result "auth") "should contain auth text"))

(test-case "W1.3: extract-recent-text returns #f for empty messages"
  (define result (extract-recent-text '() 3))
  (check-false result))

(test-case "W1.3: extract-recent-text truncates to 200 chars"
  (define long-text (make-string 300 #\x))
  (define msgs
    (list
     (make-message "m1" #f 'assistant 'assistant-msg (list (make-text-part long-text)) 100 (hasheq))))
  (define result (extract-recent-text msgs 1))
  (check-not-false result)
  (check-true (<= (string-length result) 200) "should be truncated to 200"))

(test-case "W1.1: observe-memory-for-context accepts #:query-text without error"
  ;; Memory disabled = returns empty, but should accept the kwarg
  (define result
    (observe-memory-for-context #f ; no session-config = disabled
                                #:query-text "debugging auth-store"))
  (check-true (pair? result))
  (check-true (pair? result)))

(test-case "W1.1: observe-memory-for-context backward compat (no #:query-text)"
  (define result (observe-memory-for-context #f))
  (check-true (pair? result))
  (check-true (pair? result)))

(test-case "W1.2: inject-memory-for-context accepts #:query-text"
  (define result (inject-memory-for-context #f #:query-text "test query"))
  (check-true (pair? result)))

;; ══════════════════════════════════════════════════════════════════
;; W2: Anti-looping escalation — placeholders (implemented in W2)
;; ══════════════════════════════════════════════════════════════════

(test-case "W2.1: warnings->actions escalation — 1st repeat → warn-only"
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    (define actions (warnings->actions '("Repeated tool calls detected: 3 re-reads")))
    (check-true (andmap rollback-action? actions))
    (check-equal? (rollback-action-type (car actions)) 'warn-only)
    ;; v0.99.86: warnings->actions is now pure — does not increment counter
    (check-equal? (rollback-warning-count) 0)))

(test-case "W2.1: warnings->actions escalation — 3rd repeat → force-distill"
  (parameterize ([current-rollback-state (rollback-state 2 #f 0 '())])
    (define actions (warnings->actions '("Repeated tool calls detected: 3 re-reads")))
    (check-true (andmap rollback-action? actions))
    (check-equal? (rollback-action-type (car actions)) 'force-distill)
    ;; v0.99.86: pure — counter NOT reset by warnings->actions
    (check-equal? (rollback-warning-count) 2)))

(test-case "W2.2: exploration loop → force-distill immediately"
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    (define actions (warnings->actions '("exploration loop detected: (read edit) repeated 3 times")))
    (check-equal? (rollback-action-type (car actions)) 'force-distill)))

(test-case "W2.3: stuck → expand-context"
  (define actions (warnings->actions '("stuck: 45 messages with only 3% conclusion coverage")))
  (check-equal? (rollback-action-type (car actions)) 'expand-context))

(test-case "W2.4: rollback warning count API works"
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    (check-equal? (rollback-warning-count) 0)
    (record-rollback-warning!)
    (check-equal? (rollback-warning-count) 1)))

;; ══════════════════════════════════════════════════════════════════
;; W3: Forced reflection on tool results
;; ══════════════════════════════════════════════════════════════════

(test-case "W3.1: current-reflection-prompt-enabled parameter exists and defaults to #f"
  (check-false (current-reflection-prompt-enabled)))

(test-case "W3.2: REFLECTION-THRESHOLD-CHARS is a positive integer"
  (check-true (exact-positive-integer? REFLECTION-THRESHOLD-CHARS)))

(test-case "W3.3: build-state-awareness-preamble accepts #:reflection-event keyword"
  (define preamble (build-state-awareness-preamble 'exploration '() #:reflection-event #f))
  (check-true (message? preamble)))

(test-case "W3.4: preamble without reflection event — no reminder"
  (define preamble (build-state-awareness-preamble 'exploration '()))
  (check-true (message? preamble))
  (define text (format "~a" (message-content preamble)))
  (check-false (string-contains? text "reflection-suggested")))

(test-case "W3.5: preamble with reflection event — includes reminder"
  (define preamble
    (build-state-awareness-preamble 'exploration
                                    '()
                                    #:reflection-event
                                    (hasheq 'tools '("read") 'message "Large results")))
  (check-true (message? preamble))
  (define text (format "~a" (message-content preamble)))
  (check-true (string-contains? text "record_conclusion") "preamble includes reflection reminder"))

(test-case "W3.6: reflection event is explicit arg, not consumed-on-read"
  ;; v0.99.89: Reflection event is now an explicit keyword argument.
  ;; The caller consumes it from lifecycle-state before calling.
  (define evt (hasheq 'tools '("read")))
  (define preamble (build-state-awareness-preamble 'exploration '() #:reflection-event evt))
  (check-true (message? preamble))
  ;; The event value is not mutated by the preamble function
  (check-equal? evt (hasheq 'tools '("read"))))

;; ══════════════════════════════════════════════════════════════════
;; W4: Transition detection infrastructure
;; ══════════════════════════════════════════════════════════════════

(test-case "W4.1: ws-entry->text extracts formatted text"
  (define entry (ws-entry "/tmp/test.rkt" "msg-001" 150 (current-seconds) 'kept))
  (define text (ws-entry->text entry))
  (check-true (string? text))
  (check-true (string-contains? text "/tmp/test.rkt"))
  (check-true (string-contains? text "150")))

(test-case "W4.2: ws-entry->text includes timestamp"
  (define ts 1700000000)
  (define entry (ws-entry "/path/file.rkt" "msg-002" 200 ts 'kept))
  (define text (ws-entry->text entry))
  (check-true (string-contains? text (number->string ts))))

(test-case "W4.3: warning counter resets on state transition"
  (parameterize ([current-rollback-state (rollback-state 3 #f 0 '())])
    (check-equal? (rollback-warning-count) 3)
    ;; Simulating transition reset — the actual reset happens in turn-context.rkt
    (reset-rollback-warning-count!)
    (check-equal? (rollback-warning-count) 0)))

(test-case "W4.4: ws-entry? predicate works"
  (define entry (ws-entry "/a.rkt" "m1" 10 1000 'kept))
  (check-true (ws-entry? entry))
  (check-false (ws-entry? "not an entry")))

;; ══════════════════════════════════════════════════════════════════
;; v0.96.14: Audit hotfix tests (F1, F2, F3, F4)
;; ══════════════════════════════════════════════════════════════════

;; F4: Escalation threshold is a named constant
(test-case "F4: escalation-threshold is defined and equals 2"
  (check-equal? escalation-threshold 2))

;; F1: Stuck detection trigger in check-rollback-triggers
(test-case "F1: check-rollback-triggers fires stuck-detected at ≥6 tool calls, 0 coverage"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0
                             #:repeat-tool-count 7))
  (define stuck-warning
    (for/or ([w (in-list warnings)])
      (and (eq? (car w) 'stuck-detected) (cadr w))))
  (check-not-false stuck-warning)
  (check-true (string-contains? stuck-warning "stuck"))
  (check-true (string-contains? stuck-warning "7")))

(test-case "F1: check-rollback-triggers does NOT fire stuck at ≥6 tool calls with conclusions"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0.3
                             #:repeat-tool-count 7))
  (define stuck-warning
    (for/or ([w (in-list warnings)])
      (and (eq? (car w) 'stuck-detected) w)))
  (check-false stuck-warning "no stuck warning when conclusions exist"))

(test-case "F1: check-rollback-triggers does NOT fire stuck at <6 tool calls"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0
                             #:repeat-tool-count 5))
  (define stuck-warning
    (for/or ([w (in-list warnings)])
      (and (eq? (car w) 'stuck-detected) w)))
  (check-false stuck-warning "no stuck warning below threshold"))

;; F1: Stuck trigger → expand-context via warnings->actions
(test-case "F1: stuck trigger from check-rollback-triggers → expand-context action"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0
                             #:repeat-tool-count 8))
  (define warning-strs (map cadr warnings))
  (define actions (warnings->actions warning-strs))
  (define stuck-action
    (for/or ([a (in-list actions)])
      (and (eq? (rollback-action-type a) 'expand-context) a)))
  (check-not-false stuck-action "stuck trigger produces expand-context action"))

;; F3: Reflection event wiring — explicit keyword argument
(test-case "F3: reflection event passed as keyword arg appears in preamble"
  (define preamble
    (build-state-awareness-preamble 'implementation
                                    '()
                                    #:reflection-event (hasheq 'tools '("read" "grep"))))
  (check-true (message? preamble))
  (define text (format "~a" (message-content preamble)))
  (check-true (string-contains? text "record_conclusion")))
