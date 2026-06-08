#lang racket

;; tests/test-error-hierarchy.rkt — Error hierarchy tests for v0.47.0
;; Tests the 4-branch exception hierarchy and category-based predicates.

(require rackunit
         rackunit/text-ui
         "../util/error/errors.rkt"
         "../llm/provider-errors.rkt")

(define error-hierarchy-tests
  (test-suite "error-hierarchy"

    ;; ── q-error base ──────────────────────────────────────
    (test-case "q-error is exn:fail subtype"
      (define e (q-error "test" (current-continuation-marks) (hash)))
      (check-true (exn:fail? e))
      (check-true (q-error? e)))

    (test-case "q-error context is hash"
      (define e (q-error "test" (current-continuation-marks) (hash 'key 'value)))
      (check-equal? (hash-ref (q-error-context e) 'key) 'value))

    ;; ── Branch 1: LLM errors ─────────────────────────────
    (test-case "q-llm-error inherits from q-error"
      (define e (q-llm-error "timeout" (current-continuation-marks) (hash) 'provider-timeout))
      (check-true (exn:fail? e))
      (check-true (q-error? e))
      (check-true (q-llm-error? e))
      (check-equal? (q-llm-error-category e) 'provider-timeout))

    (test-case "provider-error inherits from q-llm-error"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-true (q-error? e))
                                         (check-true (q-llm-error? e))
                                         (check-true (provider-error? e)))])
        (raise-provider-error "test" 'timeout)))

    (test-case "error-hierarchy: provider-error has category and status-code"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-equal? (provider-error-category e) 'rate-limit)
                                         (check-equal? (provider-error-status-code e) 429))])
        (raise-provider-error "rate limited" 'rate-limit 429)))

    (test-case "llm-timeout? predicate works"
      (check-true (llm-timeout? (q-llm-error "t" (current-continuation-marks) (hash) 'timeout)))
      (check-false (llm-timeout? (q-llm-error "t" (current-continuation-marks) (hash) 'rate-limit))))

    (test-case "llm-rate-limit? predicate works"
      (check-true (llm-rate-limit? (q-llm-error "t" (current-continuation-marks) (hash) 'rate-limit)))
      (check-false (llm-rate-limit? (q-llm-error "t" (current-continuation-marks) (hash) 'timeout))))

    ;; ── Branch 2: Tool errors ─────────────────────────────
    (test-case "q-tool-error inherits from q-error"
      (define e (q-tool-error "denied" (current-continuation-marks) (hash) 'permission-denied))
      (check-true (q-error? e))
      (check-true (q-tool-error? e)))

    (test-case "tool-error inherits from q-tool-error"
      (define e (tool-error "denied" (current-continuation-marks) (hash) 'permission-denied "bash"))
      (check-true (q-error? e))
      (check-true (q-tool-error? e))
      (check-true (tool-error? e))
      (check-equal? (tool-error-tool-name e) "bash"))

    (test-case "tool-permission-denied? predicate works"
      (check-true (tool-permission-denied?
                   (q-tool-error "t" (current-continuation-marks) (hash) 'permission-denied)))
      (check-false (tool-permission-denied?
                    (q-tool-error "t" (current-continuation-marks) (hash) 'execution-timeout))))

    ;; ── Branch 3: Extension errors ────────────────────────
    (test-case "q-extension-error inherits from q-error"
      (define e (q-extension-error "load fail" (current-continuation-marks) (hash) 'load-failure))
      (check-true (q-error? e))
      (check-true (q-extension-error? e)))

    (test-case "extension-error inherits from q-extension-error"
      (define e
        (extension-error "load fail"
                         (current-continuation-marks)
                         (hash)
                         'load-failure
                         "my-ext"
                         "on-start"))
      (check-true (q-error? e))
      (check-true (q-extension-error? e))
      (check-true (extension-error? e)))

    ;; ── Branch 4: Session errors ──────────────────────────
    (test-case "q-session-error inherits from q-error"
      (define e (q-session-error "corrupt" (current-continuation-marks) (hash) 'corrupted-journal))
      (check-true (q-error? e))
      (check-true (q-session-error? e)))

    (test-case "session-error inherits from q-session-error"
      (define e
        (session-error "corrupt" (current-continuation-marks) (hash) 'corrupted-journal "sess-1"))
      (check-true (q-error? e))
      (check-true (q-session-error? e))
      (check-true (session-error? e))
      (check-equal? (session-error-session-id e) "sess-1"))

    (test-case "session-corrupted? predicate works"
      (check-true (session-corrupted?
                   (q-session-error "t" (current-continuation-marks) (hash) 'corrupted-journal)))
      (check-false (session-corrupted?
                    (q-session-error "t" (current-continuation-marks) (hash) 'write-failure))))

    ;; ── Other error types ─────────────────────────────────
    (test-case "ui-error inherits from q-error"
      (define e (ui-error "render fail" (current-continuation-marks) (hash) "scrollback"))
      (check-true (q-error? e))
      (check-true (ui-error? e)))))

(module+ main
  (run-tests error-hierarchy-tests))
(module+ test
  (run-tests error-hierarchy-tests))
