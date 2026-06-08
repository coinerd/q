#lang racket

;; @speed fast  ;; @suite security

;; tests/test-permission-denial-path.rkt — Denial-path integration tests (v0.54.4 W2)

(require rackunit
         rackunit/text-ui
         "../tools/permission-gate.rkt"
         "../tools/tool.rkt"
         (only-in "../util/tool/tool-types.rkt" make-tool-call tool-call?)
         "../tools/scheduler.rkt")

(define (run-single-tool name args-hash perm-cfg)
  (define reg (make-tool-registry))
  (register-tool! reg
                  (make-tool name
                             "Test tool"
                             (hasheq 'type "object")
                             (lambda (a ctx) (make-success-result "ok"))
                             #:dangerous?
                             (or (equal? name "bash")
                                 (equal? name "write")
                                 (equal? name "edit"))))
  (define exec-ctx (if perm-cfg
                       (make-exec-context #:permission-config perm-cfg)
                       (make-exec-context)))
  (define tool-calls (list (make-tool-call "tc-1" name args-hash)))
  (run-tool-batch tool-calls reg #:exec-context exec-ctx))

(define (result-is-error? result)
  (define results (scheduler-result-results result))
  (for/or ([v (in-list results)])
    (and (tool-result? v) (tool-result-is-error? v))))

(define (result-is-success? result)
  (define results (scheduler-result-results result))
  (for/or ([v (in-list results)])
    (and (tool-result? v) (not (tool-result-is-error? v)))))

(define denial-path-suite
  (test-suite "permission denial-path integration tests"

    (test-case "strict mode: unknown tool blocked"
      (define cfg (make-default-permission-config
                   #:policy-mode 'strict
                   #:callback (lambda (n a) #f)))
      (define result (run-single-tool "mystery-tool" (hash) cfg))
      (check-true (result-is-error? result)))

    (test-case "permissive mode: unknown tool auto-approved"
      (define cfg (make-default-permission-config #:policy-mode 'permissive))
      (define result (run-single-tool "mystery-tool" (hash) cfg))
      (check-true (result-is-success? result)))

    (test-case "needs-approval tool succeeds when approved"
      (define cfg (make-default-permission-config
                   #:policy-mode 'strict
                   #:callback (lambda (n a) (equal? n "bash"))))
      (define result (run-single-tool "bash" (hash) cfg))
      (check-true (result-is-success? result)))

    (test-case "needs-approval tool blocked when denied"
      (define cfg (make-default-permission-config
                   #:policy-mode 'strict
                   #:callback (lambda (n a) #f)))
      (define result (run-single-tool "bash" (hash) cfg))
      (check-true (result-is-error? result)))

    (test-case "auto-approved tool bypasses deny-all callback"
      (define cfg (make-default-permission-config
                   #:policy-mode 'strict
                   #:callback (lambda (n a) #f)))
      (define result (run-single-tool "read" (hash) cfg))
      (check-true (result-is-success? result)))

    (test-case "no permission config: tool executes (backward compat)"
      (define result (run-single-tool "bash" (hash) #f))
      (check-true (result-is-success? result)))))

(run-tests denial-path-suite)
