#lang racket

;; @speed fast
;; @suite security

;; BOUNDARY: integration

;; tests/test-tool-internal-gate.rkt — R-15: verify tool-execute gating

(require rackunit
         rackunit/text-ui
         (only-in "../tools/tool-struct.rkt" tool? tool-name tool-execute tool-dangerous?)
         (only-in "../tools/tool.rkt" make-tool)
         (only-in "../tools/registry-table.rkt" register-tools-from-specs! tool-specs)
         (only-in "../tools/tool-classification.rkt" tool-name-needs-approval?)
         (only-in "../tools/registry.rkt" make-tool-registry lookup-tool))

(define gate-suite
  (test-suite "Tool execute gate tests"

    ;; ── tool-internal.rkt provides tool-execute ──
    (test-case "tool-internal provides tool-execute"
      (check-pred procedure? tool-execute))

    ;; ── authoritative dangerous tool classification ──
    (test-case "approval classification identifies dangerous tools"
      (for ([name '("write" "edit" "bash" "delete-lines" "skill-route")])
        (check-true (tool-name-needs-approval? name))))

    (test-case "read-only tools are not classified as dangerous"
      (for ([name '("read" "grep" "ls")])
        (check-false (tool-name-needs-approval? name))))

    ;; ── registration marks dangerous tools ──
    (test-case "registered write tool is dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("write"))
      (define t (lookup-tool reg "write"))
      (check-true (tool-dangerous? t)))

    (test-case "registered edit tool is dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("edit"))
      (define t (lookup-tool reg "edit"))
      (check-true (tool-dangerous? t)))

    (test-case "registered bash tool is dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("bash"))
      (define t (lookup-tool reg "bash"))
      (check-true (tool-dangerous? t)))

    (test-case "registered skill-route tool is dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("skill-route"))
      (define t (lookup-tool reg "skill-route"))
      (check-true (tool-dangerous? t)))

    (test-case "registered read tool is not dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("read"))
      (define t (lookup-tool reg "read"))
      (check-false (tool-dangerous? t)))

    ;; ── make-tool with #:dangerous? ──
    (test-case "make-tool accepts #:dangerous? flag"
      (define t
        (make-tool "test"
                   "test tool"
                   (hasheq 'type "object" 'properties (hasheq))
                   (lambda (args) "ok")
                   #:dangerous? #t))
      (check-true (tool-dangerous? t)))

    (test-case "make-tool defaults to not dangerous"
      (define t
        (make-tool "safe"
                   "safe tool"
                   (hasheq 'type "object" 'properties (hasheq))
                   (lambda (args) "ok")))
      (check-false (tool-dangerous? t)))))

(run-tests gate-suite 'verbose)
