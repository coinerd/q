#lang racket/base

;; @speed fast
;; @suite security

;; tests/test-spawn-approval.rkt
;; v0.99.23 §5.3: Tests for HITL approval of dangerous subagent spawns.
;; Verifies that:
;; - requires-hitl-approval? identifies shell-exec and git-write as dangerous
;; - request-spawn-approval is permissive in non-interactive mode (no publisher)
;; - Approval denial blocks the spawn with an error result
;; - Approval allows the spawn to proceed

(require rackunit
         rackunit/text-ui
         racket/string
         racket/list
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt")

;; ── Helpers ──

;; Build a subagent-config with given capabilities
(define (make-cfg #:task [task "test task"] #:capabilities [caps #f])
  (subagent-config task "You are a test agent." 3 #f #f caps))

;; ── Test Suite ──

(define suite
  (test-suite "HITL Spawn Approval (v0.99.23 §5.3)"

    ;; ── requires-hitl-approval? ──

    (test-case "requires-hitl-approval? #t for shell-exec"
      (check-not-false (requires-hitl-approval? '(shell-exec))))

    (test-case "requires-hitl-approval? #t for git-write"
      (check-not-false (requires-hitl-approval? '(git-write))))

    (test-case "requires-hitl-approval? #t when shell-exec mixed with read-only"
      (check-not-false (requires-hitl-approval? '(read-only shell-exec))))

    (test-case "requires-hitl-approval? #f for read-only only"
      (check-false (requires-hitl-approval? '(read-only))))

    (test-case "requires-hitl-approval? #f for file-write only"
      (check-false (requires-hitl-approval? '(file-write))))

    (test-case "requires-hitl-approval? #f for #f (no capabilities)"
      (check-false (requires-hitl-approval? #f)))

    (test-case "requires-hitl-approval? #f for empty list"
      (check-false (requires-hitl-approval? '())))

    (test-case "requires-hitl-approval? #t for shell-exec + git-write combo"
      (check-not-false (requires-hitl-approval? '(shell-exec git-write))))

    ;; ── request-spawn-approval ──

    (test-case "request-spawn-approval returns #t with no exec-ctx (non-interactive)"
      (check-true (request-spawn-approval '(shell-exec) "dangerous task" #f)))

    (test-case "request-spawn-approval returns #t by default (permissive)"
      (check-true (request-spawn-approval '(shell-exec git-write) "task" #f)))

    (test-case "request-spawn-approval returns #f when parameterized to deny"
      (parameterize ([current-spawn-approval-result #f])
        (check-false (request-spawn-approval '(shell-exec) "dangerous task" #f))))

    (test-case "request-spawn-approval emits event when publisher present"
      (define events-received '())
      (define mock-publisher
        (lambda (event-type payload)
          (set! events-received (cons (cons event-type payload) events-received))))
      (define mock-ctx (make-exec-context #:event-publisher mock-publisher))
      (define approved (request-spawn-approval '(shell-exec) "my task" mock-ctx))
      (check-true approved "default should be permissive")
      (check-true (pair? events-received) "should have emitted an event")
      (check-equal? (caar events-received) "mas.spawn-approval-requested"))

    (test-case "request-spawn-approval truncates long task descriptions"
      (define events-received '())
      (define mock-publisher
        (lambda (event-type payload)
          (set! events-received (cons (cons event-type payload) events-received))))
      (define mock-ctx (make-exec-context #:event-publisher mock-publisher))
      (define long-task (make-string 500 #\X))
      (request-spawn-approval '(shell-exec) long-task mock-ctx)
      (define preview (hash-ref (cdar events-received) 'task-preview ""))
      (check-true (<= (string-length preview) 200)))

    ;; ── Integration: run-subagent-with-config ──

    (test-case "denial blocks spawn with error result"
      (parameterize ([current-spawn-approval-result #f])
        (define cfg (make-cfg #:task "dangerous task" #:capabilities '(shell-exec)))
        (define result (run-subagent-with-config cfg #f))
        (check-true (tool-result? result) "should return a tool-result")
        (check-true (tool-result-is-error? result) "should be an error result")))

    (test-case "denied work does not consume spawn rate budget"
      (define timestamps (box '()))
      (parameterize ([current-spawn-approval-result #f]
                     [current-spawn-timestamps timestamps])
        (define result
          (run-subagent-with-config (make-cfg #:task "denied" #:capabilities '(shell-exec)) #f))
        (check-true (tool-result-is-error? result))
        (check-equal? (unbox timestamps) '())))

    (test-case "rate limiter counts recent timestamps and prunes expired timestamps"
      (define now (current-inexact-milliseconds))
      (define recent (box (make-list 30 now)))
      (define events '())
      (define ctx
        (make-exec-context #:event-publisher (lambda (type payload)
                                               (set! events (cons (cons type payload) events)))))
      (parameterize ([current-spawn-timestamps recent])
        (define blocked
          (run-subagent-with-config (make-cfg #:task "dangerous" #:capabilities '(shell-exec)) ctx))
        (check-true (tool-result-is-error? blocked))
        (check-equal? events '()))
      (define expired (box (make-list 30 (- now 61000))))
      (parameterize ([current-spawn-timestamps expired])
        (define allowed
          (run-subagent-with-config (make-cfg #:task "safe" #:capabilities '(read-only)) #f))
        (check-false (tool-result-is-error? allowed))))

    (test-case "non-dangerous spawn not blocked even with denial parameter"
      (parameterize ([current-spawn-approval-result #f])
        ;; read-only caps don't trigger approval, so spawn proceeds
        (define cfg (make-cfg #:task "safe task" #:capabilities '(read-only)))
        (define result (run-subagent-with-config cfg #f))
        (check-true (tool-result? result))
        ;; Should NOT be the approval-denied error
        (when (tool-result-is-error? result)
          (define content (tool-result-content result))
          (define text
            (if (pair? content)
                (hash-ref (car content) 'text "")
                ""))
          (check-false (string-contains? text "approval denied")))))))

(run-tests suite)
