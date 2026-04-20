#lang racket

(require rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt")

(define provider-error-tests
  (test-suite
   "provider-errors"

   (test-case "provider-error is exn:fail subtype"
     (check-exn exn:fail?
                (lambda () (raise-provider-error "test" 'timeout 408))))

   (test-case "provider-error has category and status-code"
     (with-handlers ([provider-error?
                      (lambda (e)
                        (check-equal? (provider-error-category e) 'rate-limit)
                        (check-equal? (provider-error-status-code e) 429))])
       (raise-provider-error "rate limited" 'rate-limit 429)
       (check-false "should not reach here")))

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

   (test-case "raise-provider-error without status-code"
     (with-handlers ([provider-error?
                      (lambda (e)
                        (check-equal? (provider-error-category e) 'timeout)
                        (check-false (provider-error-status-code e)))])
       (raise-provider-error "timed out" 'timeout)
       (check-false "should not reach here")))))

(module+ main (run-tests provider-error-tests))
(module+ test (run-tests provider-error-tests))
