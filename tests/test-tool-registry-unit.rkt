#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tool-registry-unit.rkt — T-1b tests for tools/registry.rkt

(require rackunit
         rackunit/text-ui
         (only-in "../tools/registry.rkt"
                  make-tool-registry
                  tool-registry?
                  register-tool!
                  unregister-tool!
                  lookup-tool
                  list-tools
                  tool-names
                  tool->jsexpr
                  set-active-tools!
                  tool-active?
                  list-active-tools
                  list-active-tools-jsexpr
                  with-registry-snapshot)
         (only-in "../tools/tool.rkt" make-tool)
         (only-in "../tools/registry-table/skill-tools.rkt" skill-tool-specs)
         (only-in "../tools/registry-table/spec.rkt" tool-spec-name tool-spec-schema))

;; Helper: create a real tool for registry tests
(define (make-test-tool name)
  (make-tool name
             "test tool"
             (hasheq 'type "object" 'properties (hasheq))
             (lambda (args) (hasheq 'result "ok"))))

;; Helper: create a minimal fake tool for testing
(define (make-fake-tool name [desc "fake tool"])
  (hasheq 'name
          name
          'description
          desc
          'type
          "function"
          'parameters
          (hasheq 'type "object" 'properties (hasheq))))

;; Removed old dead stub

(define reg-suite
  (test-suite "Tool registry unit tests"

    (test-case "make-tool-registry returns a tool-registry?"
      (define reg (make-tool-registry))
      (check-pred tool-registry? reg))

    (test-case "empty registry has no tools"
      (define reg (make-tool-registry))
      (check-equal? (list-tools reg) '())
      (check-equal? (tool-names reg) '()))

    (test-case "lookup-tool on empty registry returns #f"
      (define reg (make-tool-registry))
      (check-false (lookup-tool reg "nonexistent"))
      (check-false (lookup-tool reg #f)))

    (test-case "register non-tool raises error"
      (define reg (make-tool-registry))
      (check-exn exn:fail? (lambda () (register-tool! reg "not-a-tool"))))

    (test-case "register + lookup tool"
      (define reg (make-tool-registry))
      (define t (make-test-tool "test-tool"))
      (register-tool! reg t)
      (check-equal? (length (list-tools reg)) 1)
      (check-equal? (length (tool-names reg)) 1)
      (define found (lookup-tool reg "test-tool"))
      (check-not-false found))

    (test-case "unregister tool removes it"
      (define reg (make-tool-registry))
      (define t (make-test-tool "to-remove"))
      (register-tool! reg t)
      (check-equal? (length (list-tools reg)) 1)
      (unregister-tool! reg "to-remove")
      (check-false (lookup-tool reg "to-remove"))
      (check-equal? (length (list-tools reg)) 0))

    (test-case "register duplicate overwrites"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "dup"))
      (register-tool! reg (make-test-tool "dup"))
      (check-equal? (length (list-tools reg)) 1))

    (test-case "tool->jsexpr serializes tool"
      (define t (make-test-tool "ser-tool"))
      (define j (tool->jsexpr t))
      (check-equal? (hash-ref j 'type) "function")
      (check-equal? (hash-ref (hash-ref j 'function) 'name) "ser-tool"))

    (test-case "with-registry-snapshot returns snapshot"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "snap-tool"))
      (define result (with-registry-snapshot reg (lambda (snap) (sort (hash-keys snap) string<?))))
      (check-equal? result '("snap-tool")))

    (test-case "set-active-tools! with #f clears filter"
      (define reg (make-tool-registry))
      (set-active-tools! reg #f)
      (check-true (tool-active? reg "anything")))

    (test-case "set-active-tools! with list restricts"
      (define reg (make-tool-registry))
      (set-active-tools! reg '("foo" "bar"))
      (check-true (tool-active? reg "foo"))
      (check-false (tool-active? reg "baz")))

    (test-case "list-active-tools-jsexpr on non-empty registry"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "active-tool"))
      (set-active-tools! reg #f) ;; no filter = all active
      (check-equal? (length (list-active-tools-jsexpr reg)) 1))

    (test-case "list-active-tools with filter"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "a"))
      (register-tool! reg (make-test-tool "b"))
      (set-active-tools! reg '("a"))
      (check-equal? (length (list-active-tools reg)) 1))

    (test-case "spawn-subagents schema matches bounded immutable batch contract"
      (define spec
        (findf (lambda (candidate) (string=? (tool-spec-name candidate) "spawn-subagents"))
               skill-tool-specs))
      (define schema (tool-spec-schema spec))
      (define properties (hash-ref schema 'properties))
      (define jobs-schema (hash-ref properties 'jobs))
      (define item-schema (hash-ref jobs-schema 'items))
      (define item-properties (hash-ref item-schema 'properties))
      (check-equal? (hash-ref jobs-schema 'minItems) 1)
      (check-equal? (hash-ref jobs-schema 'maxItems) 12)
      (check-equal? (hash-ref item-schema 'required) '("task"))
      (for ([name (in-list '(task role model max-turns jobId capabilities))])
        (check-true (hash-has-key? item-properties name)))
      (check-false
       (member "any" (hash-ref (hash-ref (hash-ref item-properties 'capabilities) 'items) 'enum)))
      (check-equal? (hash-ref (hash-ref properties 'maxParallel) 'minimum) 1)
      (check-equal? (hash-ref (hash-ref properties 'maxParallel) 'maximum) 3))

    (test-case "tool->jsexpr requires tool? struct"
      (check-exn exn:fail? (lambda () (tool->jsexpr "not-a-tool"))))))

(run-tests reg-suite)
