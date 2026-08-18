#lang racket/base

;; tests/test-w3-capability-enforcement.rkt
;; v0.99.66 W3: Capability enforcement in preflight (Finding #3).
;;
;; Verifies that tool-required-capability is checked against the immutable
;; authority snapshot carried by the execution context.

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: integration

(require rackunit
         racket/match
         "../tools/tool.rkt"
         "../tools/scheduler-preflight.rkt"
         "../tools/scheduler.rkt"
         "../tools/permission-gate.rkt"
         "../util/capability.rkt")

;; A no-op hook dispatcher (no transformations, nothing blocked).
;; Signature: (hook-dispatcher event-symbol tool-call) -> tool-call | #f | hook-result
(define (identity-hook-dispatcher event tc)
  #f)

;; A test tool that always succeeds when called.
(define (make-dummy-tool name cap)
  (make-tool name
             "dummy tool"
             (hasheq 'type "object" 'properties (hasheq) 'required '())
             (lambda (args) (make-tool-result "ok" #f #f))
             #:required-capability cap))

(define (make-call tool-name)
  (make-tool-call #f tool-name (hasheq)))

(define (registry-with . tools)
  (define r (make-tool-registry))
  (for ([t (in-list tools)])
    (register-tool! r t))
  r)

(define (run tc reg caps)
  (run-preflight (list tc) reg identity-hook-dispatcher (make-exec-context #:capabilities caps)))

;; ============================================================
;; W3: Capability Enforcement in Preflight (v0.99.66 Finding #3)
;; ============================================================

(test-case "W3: read-only tool passes when read-only is granted"
  (define r (registry-with (make-dummy-tool "lookup" 'read-only)))
  (define result (run (make-call "lookup") r '(read-only)))
  (check-equal? (preflight-entry-status (car result)) 'ready))

(test-case "W3: read-only tool blocked when only shell-exec granted"
  (define r (registry-with (make-dummy-tool "lookup" 'read-only)))
  (define result (run (make-call "lookup") r '(shell-exec)))
  (check-equal? (preflight-entry-status (car result)) 'blocked)
  (check-regexp-match #rx"capability" (preflight-entry-error-message (car result))))

(test-case "W3: shell-exec tool blocked when only read-only granted"
  (define r (registry-with (make-dummy-tool "run" 'shell-exec)))
  (define result (run (make-call "run") r '(read-only)))
  (check-equal? (preflight-entry-status (car result)) 'blocked))

(test-case "W3: file-write tool blocked when only read-only granted"
  (define r (registry-with (make-dummy-tool "write" 'file-write)))
  (define result (run (make-call "write") r '(read-only)))
  (check-equal? (preflight-entry-status (car result)) 'blocked))

(test-case "W3: default-any tool is blocked by restricted and empty grants"
  (define r (registry-with (make-dummy-tool "dynamic" 'any)))
  (check-equal? (preflight-entry-status (car (run (make-call "dynamic") r '()))) 'blocked)
  (check-equal? (preflight-entry-status (car (run (make-call "dynamic") r '(read-only)))) 'blocked))

(test-case "W3: session with 'any grants all capabilities (backward compat)"
  (define r
    (registry-with (make-dummy-tool "read" 'any)
                   (make-dummy-tool "write" 'file-write)
                   (make-dummy-tool "bash" 'shell-exec)))
  (define results (run (make-call "write") r '(any)))
  (check-equal? (preflight-entry-status (car results)) 'ready))

(test-case "W3: multi-capability session grants matching tools"
  (define r (registry-with (make-dummy-tool "read" 'read-only) (make-dummy-tool "write" 'file-write)))
  (define results (run (make-call "write") r '(read-only file-write)))
  (check-equal? (preflight-entry-status (car results)) 'ready))

(test-case "W3: preflight ignores ambient authority when context is restricted"
  (define r (registry-with (make-dummy-tool "run" 'shell-exec)))
  (define restricted (make-exec-context #:capabilities '(read-only)))
  (define result
    (parameterize ([current-session-capabilities '(any)])
      (run-preflight (list (make-call "run")) r identity-hook-dispatcher restricted)))
  (check-equal? (preflight-entry-status (car result)) 'blocked))

(test-case "W3: scheduler planning uses problem context authority"
  (define r (registry-with (make-dummy-tool "run" 'shell-exec)))
  (define restricted (make-exec-context #:capabilities '(read-only)))
  (define plan
    (parameterize ([current-session-capabilities '(any)])
      (plan-tool-batch (scheduler-problem (list (make-call "run")) r #f #f restricted #f))))
  (check-equal? (preflight-entry-status (car (scheduler-plan-entries plan))) 'blocked))

(test-case "W3: execution backstop uses execution context authority"
  (define executions (box 0))
  (define t
    (make-tool "run"
               "dummy tool"
               (hasheq 'type "object" 'properties (hasheq) 'required '())
               (lambda (_args _ctx)
                 (set-box! executions (add1 (unbox executions)))
                 (make-success-result "ran"))
               #:required-capability 'shell-exec))
  (define r (registry-with t))
  (define permissive (make-exec-context #:capabilities '(any)))
  (define plan (plan-tool-batch (scheduler-problem (list (make-call "run")) r #f #f permissive #f)))
  (define restricted
    (make-exec-context #:capabilities '(read-only)
                       #:permission-config (make-permissive-permission-config)))
  (define result (execute-tool-plan plan restricted #f #f #f))
  (check-true (tool-result-is-error? (car (scheduler-result-results result))))
  (check-equal? (unbox executions) 0))
