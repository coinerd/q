#lang racket/base

;; tests/test-w3-capability-enforcement.rkt
;; v0.99.66 W3: Capability enforcement in preflight (Finding #3).
;;
;; Verifies that tool-required-capability is checked in the preflight stage
;; against current-session-capabilities, so a tool whose required capability
;; is not granted to the session is blocked with a descriptive message.

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         racket/match
         "../tools/tool.rkt"
         "../tools/scheduler-preflight.rkt"
         "../util/capability.rkt")

;; A no-op hook dispatcher (no transformations, nothing blocked).
;; Signature: (hook-dispatcher event-symbol tool-call) -> tool-call | #f | hook-result
(define (identity-hook-dispatcher event tc) #f)

;; A test tool that always succeeds when called.
(define (make-dummy-tool name cap)
  (make-tool name
             "dummy tool"
             (hasheq 'type "object"
                     'properties (hasheq)
                     'required '())
             (lambda (args) (make-tool-result "ok" #f #f))
             #:required-capability cap))

(define (make-call tool-name)
  (make-tool-call #f tool-name (hasheq)))

(define (registry-with . tools)
  (define r (make-tool-registry))
  (for ([t (in-list tools)]) (register-tool! r t))
  r)

(define (run tc reg caps)
  ;; Run preflight with a specific capability set active.
  (parameterize ([current-session-capabilities caps])
    (run-preflight (list tc) reg identity-hook-dispatcher)))

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

(test-case "W3: any-capability tool always passes (empty grants)"
  (define r (registry-with (make-dummy-tool "read" 'any)))
  (define result (run (make-call "read") r '()))
  (check-equal? (preflight-entry-status (car result)) 'ready))

(test-case "W3: session with 'any grants all capabilities (backward compat)"
  (define r (registry-with
              (make-dummy-tool "read" 'any)
              (make-dummy-tool "write" 'file-write)
              (make-dummy-tool "bash" 'shell-exec)))
  (define results (run (make-call "write") r '(any)))
  (check-equal? (preflight-entry-status (car results)) 'ready))

(test-case "W3: multi-capability session grants matching tools"
  (define r (registry-with
              (make-dummy-tool "read" 'read-only)
              (make-dummy-tool "write" 'file-write)))
  (define results (run (make-call "write") r '(read-only file-write)))
  (check-equal? (preflight-entry-status (car results)) 'ready))
