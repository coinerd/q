#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-capability-filtered-registry.rkt — Tests for tool capability filtering
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  list-tools
                  list-active-tools
                  tools-for-capability
                  tool-required-capability
                  tool-name
                  tool?)
         (only-in "../tools/registry-table.rkt"
                  register-tools-from-specs!
                  tool-specs
                  tool-spec-required-capability
                  tool-spec-name)
         (only-in "../agent/capability.rkt" valid-capability?))

(define suite
  (test-suite "MAS Capability Filtered Registry"

    ;; ── tool struct: required-capability field ──

    (test-case "make-tool: default required-capability is 'any"
      (define t (make-tool "test" "desc" (hasheq 'type "object") (lambda (args) "ok")))
      (check-equal? (tool-required-capability t) 'any))

    (test-case "make-tool: explicit required-capability"
      (define t
        (make-tool "test"
                   "desc"
                   (hasheq 'type "object")
                   (lambda (args) "ok")
                   #:required-capability 'shell-exec))
      (check-equal? (tool-required-capability t) 'shell-exec))

    (test-case "make-tool: required-capability accepts all valid symbols"
      (for ([cap (in-list '(read-only plan-write
                                      shell-exec
                                      file-write
                                      git-write
                                      network
                                      memory-write
                                      browser
                                      subagent
                                      any))])
        (define t
          (make-tool (format "test-~a" cap)
                     "desc"
                     (hasheq 'type "object")
                     (lambda (args) "ok")
                     #:required-capability cap))
        (check-equal? (tool-required-capability t) cap)))

    ;; ── tools-for-capability filtering ──

    (test-case "tools-for-capability: empty registry returns empty"
      (define reg (make-tool-registry))
      (check-equal? (tools-for-capability reg 'read-only) '()))

    (test-case "tools-for-capability: filters by capability"
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "reader"
                                 "read tool"
                                 (hasheq 'type "object")
                                 (lambda (args) "ok")
                                 #:required-capability 'read-only))
      (register-tool! reg
                      (make-tool "writer"
                                 "write tool"
                                 (hasheq 'type "object")
                                 (lambda (args) "ok")
                                 #:required-capability 'file-write))
      (define result (tools-for-capability reg 'read-only))
      (check-equal? (length result) 1)
      (check-equal? (tool-name (car result)) "reader"))

    (test-case "tools-for-capability: 'any tools pass through all filters"
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "reader"
                                 "read tool"
                                 (hasheq 'type "object")
                                 (lambda (args) "ok")
                                 #:required-capability 'read-only))
      (register-tool! reg
                      (make-tool "writer"
                                 "write tool"
                                 (hasheq 'type "object")
                                 (lambda (args) "ok")
                                 #:required-capability 'file-write))
      (register-tool! reg
                      (make-tool "general"
                                 "general tool"
                                 (hasheq 'type "object")
                                 (lambda (args) "ok")
                                 #:required-capability 'any))
      ;; 'any required-capability passes through read-only filter
      (define result (tools-for-capability reg 'read-only))
      (check-equal? (length result) 2)
      (define names (map tool-name result))
      (check-not-false (member "reader" names))
      (check-not-false (member "general" names)))

    (test-case "tools-for-capability: rejects invalid capability"
      (define reg (make-tool-registry))
      (check-exn exn:fail? (lambda () (tools-for-capability reg 'bogus-capability))))

    ;; ── tool-spec required-capability ──

    (test-case "tool-specs: all specs have 'any default"
      (for ([spec (in-list tool-specs)])
        (check-equal? (tool-spec-required-capability spec)
                      'any
                      (format "spec ~a should default to 'any" (tool-spec-name spec)))))

    ;; ── Integration: register-tools-from-specs! ──

    (test-case "register-tools-from-specs!: tools get required-capability from spec"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define tools (list-tools reg))
      (for ([t (in-list tools)])
        (check-equal? (tool-required-capability t)
                      'any
                      (format "tool ~a should have 'any capability" (tool-name t)))))))

(run-tests suite)
