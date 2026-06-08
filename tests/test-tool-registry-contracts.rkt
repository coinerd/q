#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: contract

;; tests/test-tool-registry-contracts.rkt -- R3: registry contract boundary tests

(require rackunit
         rackunit/text-ui
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt" make-tool))

(define contract-suite
  (test-suite "tool registry contract boundaries"

    (test-case "tool-registry-contracts: register-tool! rejects non-tool"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract?
                 (lambda () (register-tool! reg "not-a-tool"))))

    (test-case "lookup-tool with #f returns #f"
      (define reg (make-tool-registry))
      (check-equal? (lookup-tool reg #f) #f))

    (test-case "set-active-tools! rejects non-strings"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract?
                 (lambda () (set-active-tools! reg '(1 2 3)))))

    (test-case "tool-active? requires string name"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract?
                 (lambda () (tool-active? reg 123))))

    (test-case "unregister-tool! requires string name"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract?
                 (lambda () (unregister-tool! reg 42))))

    (test-case "make-tool-registry returns registry"
      (check-pred tool-registry? (make-tool-registry)))

    (test-case "tool-names returns strings"
      (define reg (make-tool-registry))
      (define t (make-tool "test" "desc" (hasheq 'type "object" 'properties (hasheq)) void))
      (register-tool! reg t)
      (define names (tool-names reg))
      (check-true (andmap string? names)))

    (test-case "list-active-tools-jsexpr returns hashes"
      (define reg (make-tool-registry))
      (define t (make-tool "test" "desc" (hasheq 'type "object" 'properties (hasheq)) void))
      (register-tool! reg t)
      (define js (list-active-tools-jsexpr reg))
      (check-true (andmap hash? js)))))

(run-tests contract-suite 'verbose)
