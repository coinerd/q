#lang racket

;; @speed fast
;; @suite default

;; tests/test-spawn-subagent-terminal-outcomes.rkt
;; W1 (TMUX-04): Typed terminal outcomes, provider request two, safe metadata.
;;
;; Verifies:
;; 1. Provider request two contains canonical tool-call + matching tool-result.
;; 2. Unique child sentinel reaches spawn result.
;; 3. Typed terminal outcomes (completed, approved-empty, timed-out).
;; 4. Safe result metadata (digest, size, session-id, terminal-status).
;; 5. No <unsupported:...> or #hasheq garbage in result text.

(require rackunit
         rackunit/text-ui
         json
         racket/generator
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../runtime/settings.rkt"
         "../util/cancellation.rkt"
         (only-in "../agent/blackboard.rkt" current-blackboard))

;; ============================================================
;; Request-capturing provider
;; ============================================================

;; Captures every request sent to the provider so tests can inspect request two.
(define (make-request-capturing-provider responses)
  (define captured-requests (box '()))
  (define idx (box 0))
  (define (send-handler req)
    (set-box! captured-requests (append (unbox captured-requests) (list req)))
    (define i (unbox idx))
    (set-box! idx (add1 i))
    (if (< i (length responses))
        (list-ref responses i)
        (make-model-response (list (hasheq 'type "text" 'text "done"))
                             (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
                             "test-model"
                             'stop)))
  (values (make-provider (lambda () "request-capture")
                         (lambda () (hasheq 'streaming #f))
                         send-handler
                         (lambda (req)
                           (generator () (yield (make-stream-chunk "done" #f #f #t)) (yield #f))))
          captured-requests))

;; ============================================================
;; Tests
;; ============================================================

(define terminal-tests
  (test-suite "spawn-subagent typed terminal outcomes (W1 TMUX-04)"

    ;; ---------------------------------------------------------
    ;; 1. Provider request two contains matching tool-call/result IDs
    ;; ---------------------------------------------------------
    (test-case "request two contains matching tool-call and tool-result IDs"
      (define-values (provider captured)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text "Let me check.")
                                          (hasheq 'type
                                                  "tool-call"
                                                  'id
                                                  "call_xyz789"
                                                  'name
                                                  "bash"
                                                  'arguments
                                                  (hasheq 'command "echo CHILD_SENTINEL_42")))
                                    (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20)
                                    "test-model"
                                    'tool-calls)
               (make-model-response
                (list (hasheq 'type "text" 'text "Task done. CHILD_SENTINEL_42 found."))
                (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20)
                "test-model"
                'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "req-two-test"))
      (define result (tool-spawn-subagent (hasheq 'task "find sentinel") ctx))
      (check-false (tool-result-is-error? result))

      (define reqs (unbox captured))
      (check-true (>= (length reqs) 2) "should have at least 2 requests")
      (define req-two (cadr reqs))
      (define messages (model-request-messages req-two))

      (define assistant
        (findf (lambda (message) (pair? (hash-ref message 'tool_calls '()))) messages))
      (define tool-message
        (findf (lambda (message) (equal? (hash-ref message 'role #f) "tool")) messages))
      (check-not-false assistant "request two must preserve the assistant tool call")
      (check-not-false tool-message "request two must contain a canonical tool message")
      (check-equal? (hash-ref (car (hash-ref assistant 'tool_calls)) 'id) "call_xyz789")
      (check-equal? (hash-ref tool-message 'tool_call_id #f)
                    "call_xyz789"
                    "tool result must have matching message-level tool_call_id"))

    ;; ---------------------------------------------------------
    ;; 2. Unique child sentinel reaches spawn result
    ;; ---------------------------------------------------------
    (test-case "child sentinel reaches spawn result"
      (define-values (provider _c)
        (make-request-capturing-provider
         (list (make-model-response
                (list (hasheq 'type "text" 'text "The answer is UNIQUE_CHILD_OUTPUT_99"))
                (hasheq 'prompt-tokens 5 'completion-tokens 10 'total-tokens 15)
                "test-model"
                'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "sentinel-test"))
      (define result (tool-spawn-subagent (hasheq 'task "produce sentinel") ctx))
      (check-false (tool-result-is-error? result))
      (define result-text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? result-text "UNIQUE_CHILD_OUTPUT_99")
                  "unique child sentinel must reach result")
      (check-false (string-contains? result-text "#hasheq"))
      (check-false (string-contains? result-text "<unsupported:completed>")))

    ;; ---------------------------------------------------------
    ;; 3. Typed terminal outcome: completed-with-result
    ;; ---------------------------------------------------------
    (test-case "completed-with-result has terminal-status 'completed'"
      (define-values (provider _c)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text "Real result text."))
                                    (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
                                    "test-model"
                                    'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "completed-test"))
      (define result (tool-spawn-subagent (hasheq 'task "produce result") ctx))
      (check-false (tool-result-is-error? result))
      (define details (tool-result-details result))
      (check-equal? (hash-ref details 'terminal-status #f) "completed"))

    ;; ---------------------------------------------------------
    ;; 4. Typed terminal outcome: approved-empty
    ;; ---------------------------------------------------------
    (test-case "approved-empty is terminal typed outcome, not error"
      (define-values (provider _c)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text ""))
                                    (hasheq 'prompt-tokens 5 'completion-tokens 0 'total-tokens 5)
                                    "test-model"
                                    'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "empty-test"))
      (define result (tool-spawn-subagent (hasheq 'task "produce nothing") ctx))
      (check-false (tool-result-is-error? result))
      (define details (tool-result-details result))
      (check-equal? (hash-ref details 'terminal-status #f)
                    "approved-empty"
                    "empty result must be typed 'approved-empty'")
      (check-equal? (hash-ref details 'result-present? #f) #f))

    ;; ---------------------------------------------------------
    ;; 5. Safe result metadata present
    ;; ---------------------------------------------------------
    (test-case "safe result metadata has digest, size, session-id"
      (define-values (provider _c)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text "Metadata test content."))
                                    (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
                                    "test-model"
                                    'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "meta-test"))
      (define result (tool-spawn-subagent (hasheq 'task "metadata test") ctx))
      (check-false (tool-result-is-error? result))
      (define details (tool-result-details result))
      (check-true (hash-has-key? details 'content-digest) "metadata must have content-digest")
      (check-true (hash-has-key? details 'content-size) "metadata must have content-size")
      (check-true (hash-has-key? details 'session-id) "metadata must have session-id")
      (check-true (> (hash-ref details 'content-size 0) 0)
                  "content-size must be positive for non-empty result")
      (check-true (and (string? (hash-ref details 'content-digest #f))
                       (> (string-length (hash-ref details 'content-digest "")) 0))
                  "content-digest must be non-empty string"))

    (test-case "completed result reports actual provider turns"
      (define-values (provider _captured)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text "done"))
                                    (hasheq)
                                    "test-model"
                                    'stop))))
      (define ctx
        (make-exec-context #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "finish" 'max-turns 5) ctx))
      (check-false (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'turns-used #f) 1))

    (test-case "max-turn exhaustion is a typed timed-out error"
      (define looping
        (make-model-response (list (hasheq 'type
                                           "tool-call"
                                           'id
                                           "loop-call"
                                           'name
                                           "read"
                                           'arguments
                                           (hasheq 'path "README.md")))
                             (hasheq)
                             "test-model"
                             'tool-calls))
      (define-values (provider _captured) (make-request-capturing-provider (list looping)))
      (define ctx
        (make-exec-context #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "loop" 'max-turns 1) ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "timed-out")
      (check-equal? (hash-ref (tool-result-details result) 'turns-used #f) 1))

    (test-case "tool-calls stop without an executable call fails closed"
      (define malformed
        (make-model-response (list (hasheq 'type "text" 'text "not a call"))
                             (hasheq)
                             "test-model"
                             'tool-calls))
      (define-values (provider _captured) (make-request-capturing-provider (list malformed)))
      (define ctx
        (make-exec-context #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "malformed") ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "failed"))

    (test-case "pre-cancelled child performs zero provider sends"
      (define sends (box 0))
      (define provider
        (make-provider (lambda () "cancel-provider")
                       (lambda () (hasheq))
                       (lambda (_request)
                         (set-box! sends (add1 (unbox sends)))
                         (make-model-response '() (hasheq) "test" 'stop))
                       (lambda (_request) '())))
      (define token (make-cancellation-token))
      (cancel-token! token)
      (define ctx
        (make-exec-context #:cancellation-token token
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "cancel") ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "cancelled")
      (check-equal? (unbox sends) 0))

    (test-case "ordinary provider exception is one safe failed terminal outcome"
      (define events (box '()))
      (define provider
        (make-provider (lambda () "failing")
                       (lambda () (hasheq))
                       (lambda (_request) (error 'provider "api_key=PROVIDER-SECRET-SENTINEL"))
                       (lambda (_request) '())))
      (define ctx
        (make-exec-context #:event-publisher
                           (lambda (event-type payload)
                             (set-box! events (cons (cons event-type payload) (unbox events))))
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "fail") ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "failed")
      (check-equal? (hash-ref (tool-result-details result) 'turns-used #f) 1)
      (check-false (string-contains? (format "~s" result) "PROVIDER-SECRET-SENTINEL"))
      (define terminal-events
        (filter (lambda (event) (equal? (car event) "subagent.terminal")) (unbox events)))
      (check-equal? (length terminal-events) 1)
      (check-equal? (hash-ref (cdar terminal-events) 'terminal-status #f) "failed"))

    (test-case "provider cancellation during retryable failure stops retries and stays cancelled"
      (define sends (box 0))
      (define token (make-cancellation-token))
      (define provider
        (make-provider (lambda () "cancel-retry")
                       (lambda () (hasheq))
                       (lambda (_request)
                         (set-box! sends (add1 (unbox sends)))
                         (cancel-token! token)
                         (error 'cancel-retry "timeout after cancellation"))
                       (lambda (_request) '())))
      (define events (box '()))
      (define ctx
        (make-exec-context #:cancellation-token token
                           #:event-publisher
                           (lambda (event-type payload)
                             (set-box! events (cons (cons event-type payload) (unbox events))))
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "cancel retry") ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "cancelled")
      (check-equal? (unbox sends) 1)
      (check-equal? (length (filter (lambda (event) (equal? (car event) "subagent.terminal"))
                                    (unbox events)))
                    1))

    (test-case "cancellation after provider response prevents tool and next turn"
      (define sends (box 0))
      (define token (make-cancellation-token))
      (define provider
        (make-provider (lambda () "mid-cancel")
                       (lambda () (hasheq))
                       (lambda (_request)
                         (set-box! sends (add1 (unbox sends)))
                         (cancel-token! token)
                         (make-model-response (list (hasheq 'type
                                                            "tool-call"
                                                            'id
                                                            "cancel-call"
                                                            'name
                                                            "read"
                                                            'arguments
                                                            (hasheq 'path "README.md")))
                                              (hasheq)
                                              "test"
                                              'tool-calls))
                       (lambda (_request) '())))
      (define ctx
        (make-exec-context #:cancellation-token token
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "cancel after response") ctx))
      (check-true (tool-result-is-error? result))
      (check-equal? (hash-ref (tool-result-details result) 'terminal-status #f) "cancelled")
      (check-equal? (unbox sends) 1))

    (test-case "post-creation setup exception emits exactly one failed terminal event"
      (define-values (provider _captured)
        (make-request-capturing-provider
         (list
          (make-model-response (list (hasheq 'type "text" 'text "unused")) (hasheq) "test" 'stop))))
      (define events (box '()))
      (define ctx
        (make-exec-context #:event-publisher
                           (lambda (event-type payload)
                             (set-box! events (cons (cons event-type payload) (unbox events))))
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result
        (parameterize ([current-blackboard 'malformed-blackboard])
          (tool-spawn-subagent (hasheq 'task "setup failure") ctx)))
      (check-true (tool-result-is-error? result))
      (define terminal-events
        (filter (lambda (event) (equal? (car event) "subagent.terminal")) (unbox events)))
      (check-equal? (length terminal-events) 1)
      (check-equal? (hash-ref (cdar terminal-events) 'terminal-status #f) "failed"))

    (test-case "created child emits exactly one safe terminal event"
      (define-values (provider _captured)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "text" 'text "SECRET_RESULT_SENTINEL"))
                                    (hasheq)
                                    "test-model"
                                    'stop))))
      (define events (box '()))
      (define ctx
        (make-exec-context #:event-publisher
                           (lambda (event-type payload)
                             (set-box! events (cons (cons event-type payload) (unbox events))))
                           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
      (define result (tool-spawn-subagent (hasheq 'task "emit") ctx))
      (check-false (tool-result-is-error? result))
      (define terminal-events
        (filter (lambda (event) (equal? (car event) "subagent.terminal")) (unbox events)))
      (check-equal? (length terminal-events) 1)
      (define payload (cdar terminal-events))
      (check-equal? (hash-ref payload 'terminal-status #f) "completed")
      (check-true (positive? (hash-ref payload 'content-size 0)))
      (check-true (non-empty-string? (hash-ref payload 'content-digest "")))
      (check-false (string-contains? (format "~v" payload) "SECRET_RESULT_SENTINEL")))

    (test-case "every created unsuccessful or empty outcome emits one matching terminal event"
      (define fixtures
        (list
         (list "approved-empty"
               (make-model-response (list (hasheq 'type "text" 'text "")) (hasheq) "test" 'stop)
               #f)
         (list
          "timed-out"
          (make-model-response
           (list
            (hasheq 'type "tool-call" 'id "loop" 'name "read" 'arguments (hasheq 'path "README.md")))
           (hasheq)
           "test"
           'tool-calls)
          #f)
         (list "failed"
               (make-model-response (list (hasheq 'type "text" 'text "malformed"))
                                    (hasheq)
                                    "test"
                                    'tool-calls)
               #f)
         (list "cancelled"
               (make-model-response (list (hasheq 'type "text" 'text "unused")) (hasheq) "test" 'stop)
               #t)))
      (for ([fixture (in-list fixtures)])
        (define expected-status (car fixture))
        (define-values (provider _captured) (make-request-capturing-provider (list (cadr fixture))))
        (define token (make-cancellation-token))
        (when (caddr fixture)
          (cancel-token! token))
        (define events (box '()))
        (define ctx
          (make-exec-context
           #:cancellation-token token
           #:event-publisher (lambda (event-type payload)
                               (set-box! events (cons (cons event-type payload) (unbox events))))
           #:runtime-settings (q-settings (hash) (hash) (hasheq 'provider provider))))
        (define result
          (tool-spawn-subagent (hasheq 'task "terminal" 'max-turns 1 'tools '("read")) ctx))
        (define terminal-events
          (filter (lambda (event) (equal? (car event) "subagent.terminal")) (unbox events)))
        (check-equal? (length terminal-events) 1 expected-status)
        (check-equal? (hash-ref (cdar terminal-events) 'terminal-status #f) expected-status)))

    ;; ---------------------------------------------------------
    ;; Unknown content type never exposes private reasoning
    ;; ---------------------------------------------------------
    (test-case "unknown content type with 'text extracts text, not #hasheq"
      (define-values (provider _c)
        (make-request-capturing-provider
         (list (make-model-response (list (hasheq 'type "thinking" 'text "internal reasoning")
                                          (hasheq 'type "text" 'text "Final answer visible."))
                                    (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
                                    "test-model"
                                    'stop))))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "unknown-type-test"))
      (define result (tool-spawn-subagent (hasheq 'task "test unknown type") ctx))
      (check-false (tool-result-is-error? result))
      (define result-text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-false (string-contains? result-text "#hasheq")
                   (format "must not contain #hasheq, got: ~a"
                           (substring result-text 0 (min 200 (string-length result-text)))))
      (check-false (string-contains? result-text "internal reasoning")
                   "private thinking content must not reach the child result"))))

(module+ test
  (run-tests terminal-tests))

(module+ main
  (run-tests terminal-tests))
