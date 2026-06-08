#lang racket/base

;; @speed fast  ;; @suite security

;; BOUNDARY: io

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
;; W-18: Warning port via bash-execution-config
;; ============================================================

(test-case "bash-execution-config warning-port defaults to #f"
  (define cfg (make-bash-execution-config))
  (check-false (bash-execution-config-warning-port cfg)))

(test-case "bash-execution-config warning-port can be set"
  (define test-port (open-output-string))
  (define cfg (make-bash-execution-config #:warning-port test-port))
  (check-eq? (bash-execution-config-warning-port cfg) test-port))

;; ============================================================
;; I-13: Thunk resolver for block-destructive? in config
;; ============================================================

(test-case "bash-execution-config block-destructive? accepts thunk"
  (define called? (box #f))
  (define cfg
    (make-bash-execution-config #:block? (lambda ()
                                           (set-box! called? #t)
                                           #t)))
  (check-true ((bash-execution-config-block-destructive? cfg)))
  (check-true (unbox called?)))

(test-case "bash-execution-config block-destructive? accepts plain boolean"
  (define cfg (make-bash-execution-config #:block? #f))
  (check-false (bash-execution-config-block-destructive? cfg)))
