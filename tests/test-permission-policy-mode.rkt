#lang racket

;; tests/test-permission-policy-mode.rkt — Policy mode tests (v0.54.4 W0)

(require rackunit
         rackunit/text-ui
         "../tools/permission-gate.rkt")

(define policy-mode-suite
  (test-suite "permission-gate policy-mode tests"

    ;; ── Default is strict mode ──
    (test-case "default config is strict mode"
      (define cfg (make-default-permission-config))
      (check-eq? (permission-config-policy-mode cfg) 'strict))

    ;; ── Strict mode denies unknown tools ──
    (test-case "strict mode requires approval for unknown tools"
      (define cfg (make-default-permission-config #:policy-mode 'strict))
      (check-true (tool-needs-approval? cfg "unknown-tool")))

    ;; ── Permissive mode auto-approves unknown tools ──
    (test-case "permissive mode auto-approves unknown tools"
      (define cfg (make-default-permission-config #:policy-mode 'permissive))
      (check-false (tool-needs-approval? cfg "unknown-tool")))

    ;; ── Known tools are unaffected by policy mode ──
    (test-case "auto-approved tools stay auto-approved in strict"
      (define cfg (make-default-permission-config #:policy-mode 'strict))
      (check-false (tool-needs-approval? cfg "read")))

    (test-case "needs-approval tools stay needing approval in permissive"
      (define cfg (make-default-permission-config #:policy-mode 'permissive))
      (check-true (tool-needs-approval? cfg "bash")))

    ;; ── Invalid mode is rejected by contract ──
    (test-case "invalid policy-mode is rejected by contract"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (make-default-permission-config #:policy-mode 'bogus))))

    ;; ── Policy mode is accessible ──
    (test-case "policy-mode accessor works"
      (define cfg-strict (make-default-permission-config #:policy-mode 'strict))
      (define cfg-perm (make-default-permission-config #:policy-mode 'permissive))
      (check-eq? (permission-config-policy-mode cfg-strict) 'strict)
      (check-eq? (permission-config-policy-mode cfg-perm) 'permissive))

    ;; ── Approval callback still works with policy mode ──
    (test-case "approval callback works with strict mode"
      (define cfg (make-default-permission-config
                   #:policy-mode 'strict
                   #:callback (lambda (name args) #f)))
      (check-false (request-approval cfg "bash" (hash))))


    (test-case "approval callback works with permissive mode"
      (define cfg (make-default-permission-config
                   #:policy-mode 'permissive
                   #:callback (lambda (name args) #t)))
      (check-true (request-approval cfg "unknown-tool" (hash))))))

(run-tests policy-mode-suite)
