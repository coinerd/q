#lang racket

(require rackunit
         rackunit/text-ui
         "../llm/http-helpers.rkt")

(define http-helpers-suite
  (test-suite
   "http-helpers tests"

   (test-case "extract-status-code parses string status line"
     (check-equal? (extract-status-code "HTTP/1.1 200 OK") 200)
     (check-equal? (extract-status-code "HTTP/2.0 404 Not Found") 404))

   (test-case "extract-status-code parses bytes status line"
     (check-equal? (extract-status-code #"HTTP/1.1 503 Service Unavailable") 503))

   (test-case "extract-status-code returns 0 for non-matching input"
     (check-equal? (extract-status-code "garbage") 0)
     (check-equal? (extract-status-code #"") 0))

   (test-case "http-error? returns #t for >= 400"
     (check-true (http-error? 400))
     (check-true (http-error? 500))
     (check-false (http-error? 200))
     (check-false (http-error? 301)))

   (test-case "raise-http-error! raises exn:fail"
     (check-exn exn:fail?
       (λ () (raise-http-error! "HTTP 500 error"))))

   (test-case "raise-http-error! message is preserved"
     (check-exn (λ (e) (string-contains? (exn-message e) "HTTP 500"))
       (λ () (raise-http-error! "HTTP 500 error"))))))

(run-tests http-helpers-suite)
