#lang racket

;; BOUNDARY: integration

;; tests/test-stream-error-wrapping.rkt -- A6-02 + A7-02: streaming error wrapping tests

(require rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt"
         "../llm/http-helpers.rkt"
         "../agent/streaming-message.rkt"
         "../util/protocol-types.rkt")

(define stream-error-suite
  (test-suite "streaming error wrapping"

    ;; ── Struct contract tests (A6-02) ──

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
        (raise-provider-error "transparency test" 'auth 401)))

    ;; ── Wrapping pattern tests (A7-02) ──

    ;; Test 6: Wrapping pattern converts raw exn:fail to provider-error
    (test-case "wrapping pattern: raw exn:fail becomes provider-error"
      (with-handlers ([provider-error?
                       (lambda (e)
                         (check-true (string-contains? (exn-message e) "raw error"))
                         (check-equal? (provider-error-category e) 'network))])
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (if (provider-error? e)
                               (raise e)
                               (raise (provider-error
                                       (format "Wrapped: ~a" (exn-message e))
                                       (current-continuation-marks)
                                       #f
                                       'network))))])
          (raise (exn:fail "raw error" (current-continuation-marks))))))

    ;; Test 7: Wrapping pattern passes through existing provider-error
    (test-case "wrapping pattern: provider-error passes through without re-wrapping"
      (with-handlers ([provider-error?
                       (lambda (e)
                         ;; Must see ORIGINAL provider-error, not re-wrapped
                         (check-equal? (provider-error-category e) 'timeout)
                         (check-equal? (provider-error-status-code e) 408))])
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (if (provider-error? e)
                               (raise e)
                               (raise (provider-error
                                       (format "Wrapped: ~a" (exn-message e))
                                       (current-continuation-marks)
                                       #f
                                       'network))))])
          (raise (provider-error "timeout error"
                                 (current-continuation-marks)
                                 408
                                 'timeout)))))

    ;; Test 8: check-provider-status! wraps 4xx as provider-error
    (test-case "check-provider-status! wraps 4xx as provider-error"
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (check-pred provider-error? e)
                         (check-equal? (provider-error-status-code e) 400))])
        (check-provider-status! "Test"
                                #"HTTP/1.1 400 Bad Request"
                                #"{}")))

    ;; Test 9: check-provider-status! passes 2xx through
    (test-case "check-provider-status! passes 200 through"
      (check-equal? (check-provider-status! "Test"
                                            #"HTTP/1.1 200 OK"
                                            #"{}")
                    (void)))))

(run-tests stream-error-suite 'verbose)
