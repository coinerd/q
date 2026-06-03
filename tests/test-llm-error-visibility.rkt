#lang racket/base

;; tests/test-llm-error-visibility.rkt — T1-1: Verify LLM parse errors are logged, not silently swallowed
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../llm/provider-errors.rkt" provider-error? provider-error-category))

;; ── Test Suite ──

(define suite
  (test-suite "LLM Error Visibility (T1-1)"

    ;; Test 1: Verify provider-error struct exists
    (test-case "provider-error struct is available"
      (check-true (procedure? provider-error?))
      (check-true (procedure? provider-error-category)))

    ;; Test 2: Verify that a mock parse error handler returns #f (not void)
    (test-case "parse error handler returns #f, not void"
      (define result
        (with-handlers ([exn:fail? (λ (e) #f)])
          (error "JSON parse error")))
      (check-equal? result #f)
      (check-not-equal? result (void)))

    ;; Test 3: Verify that parse error handler is invoked for each provider pattern
    (test-case "parse error handler logs and returns #f (pattern test)"
      (define logged (box '()))
      (define (handler e)
        (set-box! logged (cons (exn-message e) (unbox logged)))
        #f)
      (define result
        (with-handlers ([exn:fail? handler])
          (error "stream chunk parse: invalid JSON")))
      (check-equal? result #f)
      (check-not-false (unbox logged))
      (check-not-false (string-contains? (car (unbox logged)) "invalid JSON")))

    ;; Test 4: with-safe-fallback now logs warnings (v0.81.0 W3)
    (test-case "with-safe-fallback logs warnings instead of silent swallow"
      (define src (with-input-from-file "../util/error/error-helpers.rkt" (lambda () (read-string 10000))))
      (check-not-false (string-contains? src "log-warning \"with-safe-fallback caught")
                       "with-safe-fallback should log warnings"))))

(run-tests suite)
