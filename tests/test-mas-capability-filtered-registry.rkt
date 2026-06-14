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
         (only-in "../util/capability.rkt" valid-capability?))

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

    (test-case "tool-specs: all specs have valid capabilities"
      (for ([spec (in-list tool-specs)])
        (check-true (valid-capability? (tool-spec-required-capability spec))
                    (format "spec ~a should have valid capability, got ~a"
                            (tool-spec-name spec)
                            (tool-spec-required-capability spec)))))

    (test-case "tool-specs: no spec has 'any default anymore"
      (for ([spec (in-list tool-specs)])
        (check-not-equal? (tool-spec-required-capability spec)
                          'any
                          (format "spec ~a should not have 'any (should be specific)"
                                  (tool-spec-name spec)))))

    ;; ── Integration: register-tools-from-specs! ──

    (test-case "register-tools-from-specs!: tools get required-capability from spec"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define tools (list-tools reg))
      ;; All tools should have valid capabilities (not 'any)
      (for ([t (in-list tools)])
        (check-true (valid-capability? (tool-required-capability t))
                    (format "tool ~a should have valid capability, got ~a"
                            (tool-name t)
                            (tool-required-capability t)))))

    (test-case "capability annotation: read tool requires 'read-only"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define read-tool (findf (lambda (t) (equal? (tool-name t) "read")) (list-tools reg)))
      (check-not-false read-tool "read tool should be registered")
      (when read-tool
        (check-equal? (tool-required-capability read-tool) 'read-only)))

    (test-case "capability annotation: write tool requires 'file-write"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define write-tool (findf (lambda (t) (equal? (tool-name t) "write")) (list-tools reg)))
      (check-not-false write-tool "write tool should be registered")
      (when write-tool
        (check-equal? (tool-required-capability write-tool) 'file-write)))

    (test-case "capability annotation: bash tool requires 'shell-exec"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define bash-tool (findf (lambda (t) (equal? (tool-name t) "bash")) (list-tools reg)))
      (check-not-false bash-tool "bash tool should be registered")
      (when bash-tool
        (check-equal? (tool-required-capability bash-tool) 'shell-exec)))

    (test-case "capability annotation: spawn-subagent requires 'subagent"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define spawn-tool
        (findf (lambda (t) (equal? (tool-name t) "spawn-subagent")) (list-tools reg)))
      (check-not-false spawn-tool "spawn-subagent tool should be registered")
      (when spawn-tool
        (check-equal? (tool-required-capability spawn-tool) 'subagent)))))

(run-tests suite)
