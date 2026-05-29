#lang racket/base

;; tests/test-tool-timeout.rkt — v0.70.7 W0

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt")

(define-test-suite test-tool-timeout

  (test-case "make-tool without timeout defaults to #f"
    (define t (make-tool "test" "desc" (hasheq) (lambda (args) args)))
    (check-false (tool-timeout-seconds t)))

  (test-case "make-tool with timeout-seconds"
    (define t (make-tool "test" "desc" (hasheq) (lambda (args) args)
                         #:timeout-seconds 30))
    (check-equal? (tool-timeout-seconds t) 30))

  (test-case "tool->jsexpr includes timeoutSeconds when set"
    (define t (make-tool "test" "desc" (hasheq) (lambda (args) args)
                         #:timeout-seconds 60))
    (define j (tool->jsexpr t))
    (check-equal? (hash-ref (hash-ref j 'function) 'timeoutSeconds) 60))

  (test-case "tool->jsexpr omits timeoutSeconds when #f"
    (define t (make-tool "test" "desc" (hasheq) (lambda (args) args)))
    (define j (tool->jsexpr t))
    (check-false (hash-has-key? (hash-ref j 'function) 'timeoutSeconds))))

(run-tests test-tool-timeout)
