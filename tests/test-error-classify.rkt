#lang racket

(require rackunit
         rackunit/text-ui
         "../util/error-classify.rkt")

(define (make-exn msg)
  (exn:fail msg (current-continuation-marks)))

(define error-classify-suite
  (test-suite
   "error-classify tests"

   (test-case "classifies hash-ref error"
     (define r (classify-error (make-exn "hash-ref: key not found")))
     (check-not-false r)
     (check-true (string? (car r)))
     (check-true (pair? (cdr r))))

   (test-case "classifies read-json error"
     (define r (classify-error (make-exn "read-json: unexpected char")))
     (check-not-false r)
     (check string-contains? (car r) "JSON"))

   (test-case "classifies connection refused"
     (define r (classify-error (make-exn "connection refused by peer")))
     (check-not-false r)
     (check string-contains? (car r) "connect"))

   (test-case "classifies SSL error"
     (define r (classify-error (make-exn "SSL certificate verify failed")))
     (check-not-false r)
     (check string-contains? (car r) "SSL"))

   (test-case "classifies file not found"
     (define r (classify-error (make-exn "file not found: foo.txt")))
     (check-not-false r)
     (check string-contains? (car r) "file"))

   (test-case "classifies permission denied"
     (define r (classify-error (make-exn "permission denied: /etc/shadow")))
     (check-not-false r)
     (check string-contains? (car r) "Permission"))

   (test-case "classifies API key / unauthorized (401)"
     (define r (classify-error (make-exn "HTTP 401 unauthorized")))
     (check-not-false r)
     (check string-contains? (car r) "authentication"))

   (test-case "classifies rate limit (429)"
     (define r (classify-error (make-exn "rate.limit exceeded, HTTP 429")))
     (check-not-false r)
     (check string-contains? (car r) "rate"))

   (test-case "returns #f for unknown error"
     (check-false (classify-error (make-exn "something completely unexpected"))))

   (test-case "suggestions is a list of strings"
     (define r (classify-error (make-exn "hash-ref: missing")))
     (check-true (and (list? (cdr r)) (andmap string? (cdr r)))))))

(run-tests error-classify-suite)
