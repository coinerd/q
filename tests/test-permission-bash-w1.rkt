#lang racket/base

;; tests/test-permission-bash-w1.rkt — Permission contract & bash stderr tests (W-11, W-18, I-13)

(require rackunit
         racket/set
         "../tools/permission-gate.rkt"
         "../tools/builtins/bash.rkt")

;; ============================================================
;; W-11: Permission config contracts
;; ============================================================

(test-case "make-default-permission-config with valid callback"
  (define cfg (make-default-permission-config #:callback (lambda (name args) #t)))
  (check-true (permission-config? cfg)))

(test-case "make-default-permission-config with invalid callback raises error"
  (check-exn exn:fail:contract?
             (lambda () (make-default-permission-config #:callback "not-a-procedure"))))

(test-case "make-default-permission-config with valid sets"
  (define cfg
    (make-default-permission-config #:auto-approved (set "read" "ls") #:needs-approval (set "bash")))
  (check-true (permission-config? cfg))
  (check-false (tool-needs-approval? cfg "read"))
  (check-true (tool-needs-approval? cfg "bash")))

;; ============================================================
;; W-18: Warning port parameterization
;; ============================================================

(test-case "current-warning-port defaults to #f"
  (check-false (current-warning-port)))

(test-case "current-warning-port can be set"
  (define test-port (open-output-string))
  (parameterize ([current-warning-port test-port])
    (check-eq? (current-warning-port) test-port)))

;; ============================================================
;; I-13: Thunk resolver for current-block-destructive
;; ============================================================

(test-case "current-block-destructive accepts thunk"
  (define called? (box #f))
  (parameterize ([current-block-destructive (lambda ()
                                              (set-box! called? #t)
                                              #t)])
    (check-true ((current-block-destructive)))
    (check-true (unbox called?))))

(test-case "current-block-destructive accepts plain boolean"
  (parameterize ([current-block-destructive #f])
    (check-false (current-block-destructive))))
