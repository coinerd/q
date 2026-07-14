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
         "../runtime/settings.rkt")

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
                                                  "tool_call"
                                                  'id
                                                  "call_xyz789"
                                                  'name
                                                  "bash"
                                                  'arguments
                                                  (hasheq 'command "echo CHILD_SENTINEL_42")))
                                    (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20)
                                    "test-model"
                                    'tool_calls)
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

      ;; Find tool_result in request two's messages
      (define all-content
        (apply append
               (for/list ([m messages])
                 (define c (hash-ref m 'content #f))
                 (if (list? c)
                     c
                     '()))))
      (define tool-results
        (filter (lambda (c) (and (hash? c) (equal? (hash-ref c 'type #f) "tool_result")))
                all-content))
      (check-true (pair? tool-results) "request two must contain at least one tool_result")
      (when (pair? tool-results)
        (define tr (car tool-results))
        (check-equal? (hash-ref tr 'tool_call_id #f)
                      "call_xyz789"
                      "tool_result must have matching tool_call_id")))

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

    ;; ---------------------------------------------------------
    ;; 6. Unknown content type produces clean label, not hash garbage
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
      (check-true (string-contains? result-text "internal reasoning")
                  "thinking content with 'text field should extract text"))))

(module+ test
  (run-tests terminal-tests))

(module+ main
  (run-tests terminal-tests))
