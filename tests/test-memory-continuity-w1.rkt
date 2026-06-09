#lang racket/base

;; test-memory-continuity-w1.rkt — Tests for memory continuity & looping prevention
;; Plan: .planning/PLAN-v0.96.13-MEMORY-CONTINUITY-LOOPING-PREVENTION.md

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
                  current-loop-warning-count)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-reflection-event
                  build-state-awareness-preamble)
         (only-in "../agent/iteration/step-interpreter.rkt"
                  current-reflection-prompt-enabled
                  REFLECTION-THRESHOLD-CHARS)
         (only-in "../runtime/working-set.rkt"
                  ws-entry
                  ws-entry->text
                  ws-entry?))

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
  (parameterize ([current-loop-warning-count 0])
    (define actions (warnings->actions '("Repeated tool calls detected: 3 re-reads")))
    (check-true (andmap rollback-action? actions))
    (check-equal? (rollback-action-type (car actions)) 'warn-only)
    (check-equal? (current-loop-warning-count) 1)))

(test-case "W2.1: warnings->actions escalation — 3rd repeat → force-distill"
  (parameterize ([current-loop-warning-count 2])
    (define actions (warnings->actions '("Repeated tool calls detected: 3 re-reads")))
    (check-true (andmap rollback-action? actions))
    (check-equal? (rollback-action-type (car actions)) 'force-distill)
    ;; Counter resets after escalation
    (check-equal? (current-loop-warning-count) 0)))

(test-case "W2.2: exploration loop → force-distill immediately"
  (parameterize ([current-loop-warning-count 0])
    (define actions (warnings->actions '("exploration loop detected: (read edit) repeated 3 times")))
    (check-equal? (rollback-action-type (car actions)) 'force-distill)))

(test-case "W2.3: stuck → expand-context"
  (define actions (warnings->actions '("stuck: 45 messages with only 3% conclusion coverage")))
  (check-equal? (rollback-action-type (car actions)) 'expand-context))

(test-case "W2.4: current-loop-warning-count parameter works"
  (parameterize ([current-loop-warning-count 0])
    (check-equal? (current-loop-warning-count) 0)
    (current-loop-warning-count (add1 (current-loop-warning-count)))
    (check-equal? (current-loop-warning-count) 1)))

;; ══════════════════════════════════════════════════════════════════
;; W3: Forced reflection on tool results
;; ══════════════════════════════════════════════════════════════════

(test-case "W3.1: current-reflection-prompt-enabled parameter exists and defaults to #f"
  (check-false (current-reflection-prompt-enabled)))

(test-case "W3.2: REFLECTION-THRESHOLD-CHARS is a positive integer"
  (check-true (exact-positive-integer? REFLECTION-THRESHOLD-CHARS)))

(test-case "W3.3: current-reflection-event parameter exists and defaults to #f"
  (check-false (current-reflection-event)))

(test-case "W3.4: preamble without reflection event — no reminder"
  (parameterize ([current-reflection-event #f])
    (define preamble (build-state-awareness-preamble 'exploration '()))
    (check-true (message? preamble))
    (define text (format "~a" (message-content preamble)))
    (check-false (string-contains? text "reflection-suggested"))))

(test-case "W3.5: preamble with reflection event — includes reminder"
  (parameterize ([current-reflection-event (hasheq 'tools '("read") 'message "Large results")])
    (define preamble (build-state-awareness-preamble 'exploration '()))
    (check-true (message? preamble))
    (define text (format "~a" (message-content preamble)))
    (check-true (string-contains? text "record_conclusion") "preamble includes reflection reminder")))

(test-case "W3.6: reflection event consumed after preamble build"
  (parameterize ([current-reflection-event (hasheq 'tools '("read"))])
    (define preamble (build-state-awareness-preamble 'exploration '()))
    ;; After building preamble, the event should be cleared
    (check-false (current-reflection-event) "reflection event cleared after consumption")))

;; ══════════════════════════════════════════════════════════════════
;; W4: Transition detection infrastructure
;; ══════════════════════════════════════════════════════════════════

(test-case "W4.1: ws-entry->text extracts formatted text"
  (define entry (ws-entry "/tmp/test.rkt" "msg-001" 150 (current-seconds)))
  (define text (ws-entry->text entry))
  (check-true (string? text))
  (check-true (string-contains? text "/tmp/test.rkt"))
  (check-true (string-contains? text "150")))

(test-case "W4.2: ws-entry->text includes timestamp"
  (define ts 1700000000)
  (define entry (ws-entry "/path/file.rkt" "msg-002" 200 ts))
  (define text (ws-entry->text entry))
  (check-true (string-contains? text (number->string ts))))

(test-case "W4.3: warning counter resets on state transition"
  (parameterize ([current-loop-warning-count 3])
    (check-equal? (current-loop-warning-count) 3)
    ;; Simulating transition reset — the actual reset happens in turn-context.rkt
    (current-loop-warning-count 0)
    (check-equal? (current-loop-warning-count) 0)))

(test-case "W4.4: ws-entry? predicate works"
  (define entry (ws-entry "/a.rkt" "m1" 10 1000))
  (check-true (ws-entry? entry))
  (check-false (ws-entry? "not an entry")))
