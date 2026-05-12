#lang racket

;; tests/test-tool-registry-api.rkt — R-12/R-24/R-25: Tool registry API cleanup

(require rackunit
         rackunit/text-ui
         "../tools/registry.rkt"
         "../tools/tool-struct.rkt")

(define (make-test-tool name desc)
  (tool name desc (hasheq 'type "object" 'properties (hasheq)) void #f #f #f #f #f))

(define api-suite
  (test-suite "Tool registry API tests (R-12, R-24, R-25)"

    (test-case "list-active-tools-jsexpr returns empty for fresh registry"
      (define reg (make-tool-registry))
      (define result (list-active-tools-jsexpr reg))
      (check-equal? result '()))

    (test-case "list-active-tools-jsexpr returns JSON for registered tools"
      (define reg (make-tool-registry))
      (define t (make-test-tool "test-tool" "A test tool"))
      (register-tool! reg t)
      (define result (list-active-tools-jsexpr reg))
      (check = (length result) 1)
      ;; tool->jsexpr wraps in function schema format
      (define entry (car result))
      (check-equal? (hash-ref entry 'type) "function")
      (define fn-data (hash-ref entry 'function))
      (check-equal? (hash-ref fn-data 'name) "test-tool")
      (check-equal? (hash-ref fn-data 'description) "A test tool"))

    (test-case "list-tools-jsexpr returns same as list-active-tools-jsexpr"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "t1" "desc 1"))
      (register-tool! reg (make-test-tool "t2" "desc 2"))
      (check-equal? (list-tools-jsexpr reg) (list-active-tools-jsexpr reg)))))

(run-tests api-suite 'verbose)
