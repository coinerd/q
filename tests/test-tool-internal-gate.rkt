#lang racket
;; @suite security

;; BOUNDARY: integration

;; tests/test-tool-internal-gate.rkt — R-15: verify tool-execute gating

(require rackunit
         rackunit/text-ui
         (only-in "../tools/tool-struct.rkt" tool? tool-name tool-execute tool-dangerous?)
         (only-in "../tools/tool.rkt" make-tool)
         (only-in "../tools/registry-table.rkt"
                  register-tools-from-specs!
                  tool-specs
                  tool-spec-name
                  tool-spec?
                  dangerous-tool-names)
         (only-in "../tools/registry.rkt" make-tool-registry lookup-tool))

(define gate-suite
  (test-suite "Tool execute gate tests"

    ;; ── tool-internal.rkt provides tool-execute ──
    (test-case "tool-internal provides tool-execute"
      (check-pred procedure? tool-execute))

    ;; ── dangerous tool metadata ──
    (test-case "dangerous-tool-names includes write/edit/bash/delete-lines"
      (check-not-false (member "write" dangerous-tool-names))
      (check-not-false (member "edit" dangerous-tool-names))
      (check-not-false (member "bash" dangerous-tool-names))
      (check-not-false (member "delete-lines" dangerous-tool-names)))

    (test-case "non-dangerous tools not in list"
      (check-false (member "read" dangerous-tool-names))
      (check-false (member "grep" dangerous-tool-names))
      (check-false (member "ls" dangerous-tool-names)))

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

    (test-case "registered read tool is not dangerous"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs #:only '("read"))
      (define t (lookup-tool reg "read"))
      (check-false (tool-dangerous? t)))

    ;; ── make-tool with #:dangerous? ──
    (test-case "make-tool accepts #:dangerous? flag"
      (define t
        (make-tool "test" "test tool" (hasheq 'type "object") (lambda (args) "ok") #:dangerous? #t))
      (check-true (tool-dangerous? t)))

    (test-case "make-tool defaults to not dangerous"
      (define t (make-tool "safe" "safe tool" (hasheq 'type "object") (lambda (args) "ok")))
      (check-false (tool-dangerous? t)))))

(run-tests gate-suite 'verbose)
