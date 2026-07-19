#lang racket/base

;; @speed fast
;; @suite security

;; tests/test-spawn-approval.rkt
;; Tests for fail-closed, digest-bound HITL approval of subagent spawns.

(require rackunit
         rackunit/text-ui
         racket/string
         racket/list
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/builtins/spawn-execution-plan.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  approval-decide!))

;; ── Helpers ──

(define (make-cfg #:task [task "test task"] #:capabilities [caps #f])
  (subagent-config task "You are a test agent." 3 #f #f caps))

(define (make-direct-plan caps task)
  (make-spawn-execution-plan
   'single
   (hasheq 'task task 'effective-capabilities caps)
   (hasheq 'task-preview (redacted-approval-preview task) 'capabilities caps)))

(define (make-counting-context sends #:publisher [publisher #f])
  (define provider
    (make-provider
     (lambda () "counting-mock")
     (lambda () (hasheq 'streaming #f))
     (lambda (_request)
       (set-box! sends (add1 (unbox sends)))
       (make-model-response (list (hasheq 'type "text" 'text "done"))
                            (hasheq 'prompt-tokens 1 'completion-tokens 1 'total-tokens 2)
                            "counting-mock"
                            'stop))
     (lambda (_request) (error 'counting-mock "streaming not expected"))))
  (make-exec-context #:event-publisher publisher
                     #:runtime-settings (hasheq 'provider provider 'model "counting-mock")))

(define suite
  (test-suite "HITL Spawn Approval"

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

    (test-case "dangerous request without interactive broker denies"
      (clear-approval-channel!)
      (check-false (request-spawn-approval (make-direct-plan '(shell-exec) "dangerous task") #f)))

    (test-case "publisher without broker is telemetry, not approval authority"
      (clear-approval-channel!)
      (define events-received '())
      (define mock-ctx
        (make-exec-context #:event-publisher (lambda (event-type payload)
                                               (set! events-received
                                                     (cons (cons event-type payload)
                                                           events-received)))))
      (check-false (request-spawn-approval (make-direct-plan '(shell-exec) "my task") mock-ctx))
      (check-equal? (map car events-received) '("mas.spawn-approval-terminal"))
      (check-equal? (hash-ref (cdar events-received) 'terminal-status) "denied-headless"))

    (test-case "interactive publisher approves using request id and commitment digest"
      (dynamic-wind
       (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
       (lambda ()
         (define events-received '())
         (define mock-ctx
           (make-exec-context #:event-publisher
                              (lambda (event-type payload)
                                (set! events-received
                                      (append events-received (list (cons event-type payload))))
                                (when (string=? event-type "mas.spawn-approval-requested")
                                  (check-true (approval-decide! (hash-ref payload 'request-id)
                                                                (hash-ref payload 'commitment-digest)
                                                                #t))))))
         (check-not-false (request-spawn-approval (make-direct-plan '(shell-exec) "my task")
                                                  mock-ctx))
         (check-equal? (map car events-received)
                       '("mas.spawn-approval-requested" "mas.spawn-approval-terminal")))
       clear-approval-channel!))

    (test-case "request-spawn-approval carries bounded plan preview"
      (clear-approval-channel!)
      (define events-received '())
      (define mock-ctx
        (make-exec-context #:event-publisher (lambda (event-type payload)
                                               (set! events-received
                                                     (cons (cons event-type payload)
                                                           events-received)))))
      (request-spawn-approval (make-direct-plan '(shell-exec) (make-string 500 #\X)) mock-ctx)
      (define preview (hash-ref (cdar events-received) 'task-preview ""))
      (check-true (<= (string-length preview) 200)))

    ;; ── Integration: run-subagent-with-config ──

    (test-case "dangerous no-channel denial creates no child, sends, or rate effects"
      (clear-approval-channel!)
      (define sends (box 0))
      (define timestamps (box '()))
      (define result
        (parameterize ([current-spawn-timestamps timestamps])
          (run-subagent-with-config (make-cfg #:task "dangerous task" #:capabilities '(shell-exec))
                                    (make-counting-context sends))))
      (check-true (tool-result? result))
      (check-true (tool-result-is-error? result))
      (define details (tool-result-details result))
      (check-equal? (hash-ref details 'terminal-status #f) "denied")
      (check-false (hash-has-key? details 'child-id))
      (check-false (hash-has-key? details 'session-id))
      (check-equal? (unbox sends) 0)
      (check-equal? (unbox timestamps) '()))

    (test-case "omitted capabilities deny single spawn without interactive approval"
      (clear-approval-channel!)
      (define sends (box 0))
      (define timestamps (box '()))
      (define result
        (parameterize ([current-spawn-timestamps timestamps])
          (run-subagent-with-config (make-cfg #:task "bounded defaults")
                                    (make-counting-context sends))))
      (check-true (tool-result-is-error? result))
      (check-equal? (unbox sends) 0)
      (check-equal? (unbox timestamps) '()))

    (test-case "dangerous single spawn proceeds after digest-bound interactive approval"
      (define sends (box 0))
      (dynamic-wind
       (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
       (lambda ()
         (define publisher
           (lambda (type payload)
             (when (string=? type "mas.spawn-approval-requested")
               (check-true (approval-decide! (hash-ref payload 'request-id)
                                             (hash-ref payload 'commitment-digest)
                                             #t)))))
         (define result
           (run-subagent-with-config (make-cfg #:task "approved" #:capabilities '(shell-exec))
                                     (make-counting-context sends #:publisher publisher)))
         (check-false (tool-result-is-error? result))
         (check-equal? (unbox sends) 1))
       clear-approval-channel!))

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

    (test-case "terminal teardown revokes grant before single execution with zero effects"
      (define sends (box 0))
      (define timestamps (box '()))
      (define events '())
      (dynamic-wind
       (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
       (lambda ()
         (define ctx
           (make-counting-context
            sends
            #:publisher
            (lambda (type payload)
              (set! events (cons type events))
              (cond
                [(string=? type "mas.spawn-approval-requested")
                 (check-true (approval-decide! (hash-ref payload 'request-id)
                                               (hash-ref payload 'commitment-digest)
                                               #t))]
                [(string=? type "mas.spawn-approval-terminal") (clear-approval-channel!)]))))
         (parameterize ([current-spawn-timestamps timestamps])
           (define result
             (run-subagent-with-config (make-cfg #:task "revoked" #:capabilities '(shell-exec)) ctx))
           (check-true (tool-result-is-error? result))
           (check-equal? (hash-ref (tool-result-details result) 'terminal-status) "denied")
           (check-equal? (unbox sends) 0)
           (check-equal? (unbox timestamps) '())
           (check-false (member "subagent.terminal" events))))
       clear-approval-channel!))

    (test-case "safe headless spawn remains allowed"
      (clear-approval-channel!)
      (define cfg (make-cfg #:task "safe task" #:capabilities '(read-only)))
      (define result (run-subagent-with-config cfg #f))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)))))

(run-tests suite)
