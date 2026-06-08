#lang racket

;; @speed fast  ;; @suite arch

;; q/tests/test-gui-error-boundary.rkt — Tests for gui/views/error-boundary.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/error-boundary.rkt")

(define-test-suite
 test-gui-error-boundary
 (test-case "render-error-fallback produces error view"
   (define result (render-error-fallback (default-theme)))
   (check-equal? (hash-ref result 'view) 'error-fallback)
   (check-not-false (hash-ref result 'message)))
 (test-case "render-error-fallback with custom message"
   (define result (render-error-fallback (default-theme) "Custom error"))
   (check-equal? (hash-ref result 'message) "Custom error"))
 (test-case "with-error-boundary returns wrapped function"
   (define wrapped
     (with-error-boundary (lambda () (error "boom")) (lambda () (hash 'view 'fallback))))
   (check-true (procedure? wrapped)))
 (test-case "with-error-boundary catches errors"
   (define wrapped
     (with-error-boundary (lambda () (error "boom")) (lambda () (hash 'view 'fallback))))
   (define result (wrapped))
   (check-equal? (hash-ref result 'view) 'fallback))
 (test-case "with-error-boundary passes through on success"
   (define wrapped
     (with-error-boundary (lambda () (hash 'view 'success)) (lambda () (hash 'view 'fallback))))
   (define result (wrapped))
   (check-equal? (hash-ref result 'view) 'success))
 (test-case "with-error-boundary passes arguments"
   (define wrapped
     (with-error-boundary (lambda (x) (hash 'value (+ x 1))) (lambda (x) (hash 'value 0))))
   (define result (wrapped 5))
   (check-equal? (hash-ref result 'value) 6)))

(run-tests test-gui-error-boundary)
