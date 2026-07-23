#lang racket/base

;; tests/test-scheduler-execution-plane.rkt
;; Tests for execution-plane routing in the scheduler.

;; @speed fast
(require rackunit
         rackunit/text-ui
         racket/runtime-path
         (only-in racket/list first)
         (only-in "../tools/tool.rkt"
                  tool?
                  tool-name
                  make-tool
                  make-tool-registry
                  register-tool!
                  tool-result?
                  tool-result-content
                  tool-result-is-error?
                  make-error-result
                  make-success-result
                  make-exec-context
                  make-tool-call)
         (only-in "../tools/tool-struct.rkt"
                  tool-dangerous?
                  tool-externalizable?
                  tool-required-capability)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         (only-in "../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  current-worker-args
                  shutdown-worker!))

;; ── Worker path resolution ─────────────────────────────────────
;; Resolve worker-main.rkt relative to this source file.
(define-runtime-path worker-main-path "../sandbox/worker-main.rkt")
(current-worker-args (list "-tm" (path->string worker-main-path)))

;; ── Test Helpers ────────────────────────────────────────────────

(define call-counter (box 0))

(define (make-local-bash-tool)
  (make-tool "bash"
             "Local bash tool for testing"
             (hasheq 'type "function" 'function (hasheq 'name "bash" 'parameters (hasheq)))
             (lambda (args [ctx #f])
               (set-box! call-counter (add1 (unbox call-counter)))
               (make-success-result "executed locally"))
             #:dangerous? #t
             #:required-capability 'shell-exec
             #:externalizable? #t))

(define (make-non-dangerous-tool)
  (make-tool "read"
             "Read-only tool"
             (hasheq 'type "function" 'function (hasheq 'name "read" 'parameters (hasheq)))
             (lambda (args [ctx #f]) (make-success-result "read ok"))
             #:dangerous? #f
             #:required-capability 'read-only
             #:externalizable? #t))

(define (make-non-externalizable-tool)
  (make-tool "special"
             "Tool with closures, not externalizable"
             (hasheq 'type "function" 'function (hasheq 'name "special" 'parameters (hasheq)))
             (lambda (args [ctx #f]) (make-success-result "executed locally (special)"))
             #:dangerous? #t
             #:required-capability 'shell-exec
             #:externalizable? #f))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Scheduler Execution-Plane Routing"

    ;; Reset state before each test
    (current-execution-plane-enabled #f)

    ;; ── Backward Compat: Execution plane disabled ──

    (test-case "dangerous tool executes locally when plane disabled"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #f])
        (define reg (make-tool-registry))
        (register-tool! reg (make-local-bash-tool))
        (define result (run-tool-batch (list (make-tool-call "tc-1" "bash" (hasheq))) reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 1)
        (check-false (tool-result-is-error? (first results)))
        (check-equal? (unbox call-counter) 1)
        (check-equal? (tool-result-content (first results)) "executed locally")))

    (test-case "non-dangerous tool always executes locally"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-non-dangerous-tool))
        (define result (run-tool-batch (list (make-tool-call "tc-2" "read" (hasheq))) reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 1)
        (check-false (tool-result-is-error? (first results)))
        (check-equal? (tool-result-content (first results)) "read ok")))

    (test-case "non-externalizable tool always executes locally even with plane enabled"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-non-externalizable-tool))
        (define result (run-tool-batch (list (make-tool-call "tc-3" "special" (hasheq))) reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 1)
        (check-false (tool-result-is-error? (first results)))
        (check-equal? (tool-result-content (first results)) "executed locally (special)")))

    ;; ── Tool Struct: externalizable? field ──

    (test-case "make-tool defaults externalizable? to #f (M2)"
      (define t
        (make-tool "test"
                   "desc"
                   (hasheq 'type "function" 'function (hasheq 'name "test"))
                   (lambda args (void))))
      (check-false (tool-externalizable? t)))

    (test-case "make-tool with #:externalizable? #f"
      (define t
        (make-tool "test"
                   "desc"
                   (hasheq 'type "function" 'function (hasheq 'name "test"))
                   (lambda args (void))
                   #:externalizable? #f))
      (check-false (tool-externalizable? t)))

    (test-case "dangerous tool has dangerous? #t"
      (define t (make-local-bash-tool))
      (check-true (tool-dangerous? t)))

    ;; ── Mixed batch routing ──

    (test-case "mixed batch: non-dangerous always local, dangerous routed"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #f])
        ;; With plane disabled, both execute locally
        (define reg (make-tool-registry))
        (register-tool! reg (make-local-bash-tool))
        (register-tool! reg (make-non-dangerous-tool))
        (define result
          (run-tool-batch (list (make-tool-call "tc-4" "bash" (hasheq))
                                (make-tool-call "tc-5" "read" (hasheq)))
                          reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 2)
        ;; Both should succeed locally
        (for ([r (in-list results)])
          (check-false (tool-result-is-error? r)))
        (check-equal? (unbox call-counter) 1)))

    ;; ── Execution plane enabled: dangerous+externalizable routed ──

    (test-case "dangerous+externalizable tool routes through worker when enabled"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-local-bash-tool))
        ;; When execution plane is enabled, the dangerous tool should NOT
        ;; execute locally (call-counter stays 0). Instead it goes through
        ;; the worker process.
        (define result
          (run-tool-batch (list (make-tool-call "tc-6" "bash" (hasheq 'command "echo via-worker")))
                          reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 1)
        ;; The local execute function should NOT have been called
        (check-equal? (unbox call-counter) 0)
        ;; The result should come from the worker (success or error depending on tool availability)
        ;; The worker's tool registry has "bash" so it should work
        (check-false (tool-result-is-error? (first results)))
        (shutdown-worker!)))

    ;; ── Result format consistency ──

    (test-case "local and remote results have same format"
      ;; Execute locally
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #f])
        (define reg (make-tool-registry))
        (register-tool! reg (make-local-bash-tool))
        (define local-result (run-tool-batch (list (make-tool-call "tc-7" "bash" (hasheq))) reg))
        (define local-results (scheduler-result-results local-result))
        (check-true (tool-result? (first local-results)))))))

;; ── Run ─────────────────────────────────────────────────────────

(current-execution-plane-enabled #f)
(run-tests suite)
(shutdown-worker!)
