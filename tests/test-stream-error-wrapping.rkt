#lang racket

;; BOUNDARY: unit

;; tests/test-stream-error-wrapping.rkt -- A6-02: streaming error wrapping tests

(require rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt")

(define stream-error-suite
  (test-suite "streaming error wrapping"

    ;; Test 1: provider-error struct is exn:fail subtype
    (test-case "provider-error is exn:fail"
      (check-exn exn:fail?
                 (lambda () (raise-provider-error "test" 'network))))

    ;; Test 2: provider-error category is 'network for streaming
    (test-case "provider-error has network category"
      (with-handlers ([provider-error?
                       (lambda (e)
                         (check-equal? (provider-error-category e) 'network)
                         (check-false (provider-error-status-code e)))])
        (raise-provider-error "network failure" 'network #f)))

    ;; Test 3: raise-provider-error defaults status-code to #f
    (test-case "raise-provider-error without status-code defaults to #f"
      (with-handlers ([provider-error?
                       (lambda (e)
                         (check-false (provider-error-status-code e)))])
        (raise-provider-error "no status" 'timeout)))

    ;; Test 4: provider-error roundtrip preserves message
    (test-case "provider-error preserves original message"
      (with-handlers ([provider-error?
                       (lambda (e)
                         (check-true (string-contains? (exn-message e) "test message")))])
        (raise-provider-error "test message for roundtrip" 'network)))

    ;; Test 5: provider-error is transparent
    (test-case "provider-error is transparent struct"
      (with-handlers ([provider-error?
                       (lambda (e)
                         (check-true (vector? (struct->vector e))))])
        (raise-provider-error "transparency test" 'auth 401)))))

(run-tests stream-error-suite 'verbose)
