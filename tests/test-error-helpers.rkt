#lang racket/base

(require rackunit
         "../util/error-helpers.rkt")

;; with-safe-fallback returns #f on exception
(test-case "with-safe-fallback returns default on exception"
  (check-false (with-safe-fallback #f (error "boom"))))

;; with-safe-fallback returns body value on success
(test-case "with-safe-fallback returns body value on success"
  (check-equal? (with-safe-fallback #f (+ 1 2)) 3))

;; with-safe-fallback with '() default
(test-case "with-safe-fallback with '() default"
  (check-equal? (with-safe-fallback '() (error "boom")) '()))

;; with-safe-fallback preserves specific values
(test-case "with-safe-fallback with custom default"
  (check-equal? (with-safe-fallback 'not-found (error "boom")) 'not-found))

;; with-safe-fallback with multiple body expressions
(test-case "with-safe-fallback with multiple body expressions"
  (check-equal? (with-safe-fallback #f (define x 10) (+ x 5)) 15))

;; with-logged-error returns #f on exception
(test-case "with-logged-error returns #f on exception"
  (check-false (with-logged-error "test-error" (error "boom"))))

;; with-logged-error returns body value on success
(test-case "with-logged-error returns body value on success"
  (check-equal? (with-logged-error "test-error" (+ 1 2)) 3))

;; with-logged-error actually logs (smoke test — just ensure no crash)
(test-case "with-logged-error logs without crashing"
  (check-false (with-logged-error "test logging" (error "something went wrong"))))
