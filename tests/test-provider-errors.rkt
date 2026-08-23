#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: integration

(require rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt"
         "../util/error/errors.rkt")

(define provider-error-tests
  (test-suite "provider-errors"

    (test-case "provider-error is exn:fail subtype"
      (check-exn exn:fail? (lambda () (raise-provider-error "test" 'timeout 408))))

    (test-case "provider-errors: provider-error has category and status-code"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-equal? (provider-error-category e) 'rate-limit)
                                         (check-equal? (provider-error-status-code e) 429))])
        (raise-provider-error "rate limited" 'rate-limit 429)
        (check-false "should not reach here")))

    (test-case "classify-http-status maps 400 to bad-request"
      (check-equal? (classify-http-status 400) 'bad-request))

    (test-case "classify-http-status returns correct categories"
      (check-equal? (classify-http-status 401) 'auth)
      (check-equal? (classify-http-status 403) 'auth)
      (check-equal? (classify-http-status 429) 'rate-limit)
      (check-equal? (classify-http-status 500) 'server)
      (check-equal? (classify-http-status 502) 'server)
      (check-equal? (classify-http-status 200) #f)
      (check-equal? (classify-http-status 301) #f))

    (test-case "classify-http-status falls back to network for unknown 4xx"
      (check-equal? (classify-http-status 418) 'network))

    (test-case "provider-error is q-error subtype"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-true (q-error? e) "provider-error should be q-error")
                                         (check-true (provider-error? e)))])
        (raise-provider-error "test" 'timeout)
        (check-false "should not reach here")))

    (test-case "provider-error context field accessible via q-error"
      (with-handlers ([provider-error? (lambda (e) (check-true (hash? (q-error-context e))))]
                      [exn:fail? (lambda (e) (check-true (provider-error? e)))])
        (raise-provider-error "test" 'timeout)
        (check-false "should not reach here")))

    (test-case "classify-http-status handles 413 context-overflow"
      (check-equal? (classify-http-status 413) 'context-overflow))

    (test-case "raise-provider-error without status-code"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-equal? (provider-error-category e) 'timeout)
                                         (check-false (provider-error-status-code e)))])
        (raise-provider-error "timed out" 'timeout)
        (check-false "should not reach here")))

    ;; ============================================================
    ;; W6 (BUG-0011): transient-failure classification
    ;; ============================================================

    (test-case "provider-error-transient?: network 5xx / timeout / rate-limit are transient"
      (for ([cat (in-list '(server server-error network timeout rate-limit))])
        (with-handlers ([provider-error? (lambda (e)
                                           (check-true (provider-error-transient? e)
                                                       (format "~a should be transient" cat)))])
          (raise-provider-error "boom" cat 500)
          (check-false "should not reach here"))))

    (test-case "provider-error-transient?: auth / bad-request / context-overflow are not"
      (for ([cat (in-list '(auth bad-request context-overflow))])
        (with-handlers ([provider-error? (lambda (e)
                                           (check-false (provider-error-transient? e)
                                                        (format "~a should NOT be transient" cat)))])
          (raise-provider-error "boom" cat 401)
          (check-false "should not reach here"))))

    (test-case "transient-llm-failure?: classifies structured provider errors"
      (with-handlers ([provider-error? (lambda (e) (check-true (transient-llm-failure? e)))])
        (raise-provider-error "502 bad gateway" 'server 502)
        (check-false "should not reach here"))
      (with-handlers ([provider-error? (lambda (e) (check-false (transient-llm-failure? e)))])
        (raise-provider-error "unauthorized" 'auth 401)
        (check-false "should not reach here")))

    (test-case "transient-llm-failure?: SSE stall / timeout messages are transient"
      (check-true (transient-llm-failure? (exn:fail "stream stalled after 30s"
                                                    (current-continuation-marks))))
      (check-true (transient-llm-failure? (exn:fail "read timed out" (current-continuation-marks))))
      (check-true (transient-llm-failure? (exn:fail "Connection reset by peer"
                                                    (current-continuation-marks)))))

    (test-case "transient-llm-failure?: non-error values and non-transient messages are not"
      (check-false (transient-llm-failure? "just a string"))
      (check-false (transient-llm-failure? 42))
      (check-false (transient-llm-failure? #f))
      (check-false (transient-llm-failure? (exn:fail "model not found"
                                                     (current-continuation-marks)))))))

(module+ main
  (run-tests provider-error-tests))
