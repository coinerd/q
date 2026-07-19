#lang racket/base

;; @speed fast
;; @suite security

;; Correlated, digest-bound, exactly-once HITL approval delivery.

(require rackunit
         rackunit/text-ui
         json
         racket/list
         "../runtime/approval/broker.rkt"
         "../tui/state-types.rkt"
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  tui-ctx-ui-state-box
                  handle-approval-overlay-key)
         (only-in "../tools/builtins/spawn-subagent.rkt" request-spawn-approval)
         (only-in "../tools/builtins/spawn-execution-plan.rkt"
                  make-spawn-execution-plan
                  spawn-execution-plan-digest)
         "../tools/tool.rkt")

(define digest-a (make-string 64 #\a))
(define digest-b (make-string 64 #\b))
(define view-a (hasheq 'task-preview "correlated task" 'capabilities '(shell-exec)))

(define (consume-grant grant digest)
  (call-with-approval-grant grant digest (lambda () #t)))

(define (with-channel thunk #:timeout-ms [timeout-ms 500])
  (define channel (make-approval-channel #:timeout-ms timeout-ms))
  (dynamic-wind (lambda () (set-approval-channel! channel))
                (lambda () (thunk channel))
                clear-approval-channel!))

(define (make-test-plan [task "dangerous task"])
  (make-spawn-execution-plan 'single
                             (hasheq 'task task 'capabilities '(shell-exec) 'tools '("bash"))
                             (hasheq 'task-preview task 'capabilities '(shell-exec))))

(define suite
  (test-suite "Correlated Digest-Bound Approval Delivery"

    (test-case "request IDs survive JSON round trip"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define wire (jsexpr->string (hasheq 'request-id id)))
                      (define recovered-id (hash-ref (string->jsexpr wire) 'request-id))
                      (check-equal? recovered-id id)
                      (check-true (approval-decide! recovered-id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-true (consume-grant grant digest-a)))))

    (test-case "mismatched ID and digest cannot cross-deliver"
      (with-channel (lambda (channel)
                      (define id-a (register-approval-request-for-channel! channel digest-a view-a))
                      (define id-b (register-approval-request-for-channel! channel digest-b view-a))
                      (check-not-equal? id-a id-b)
                      (check-false (approval-decide! id-a digest-b #t))
                      (check-false (approval-decide! id-b digest-a #t))
                      (check-true (approval-request-pending? id-a digest-a))
                      (check-true (approval-request-pending? id-b digest-b))
                      (check-true (approval-decide! id-a digest-a #f))
                      (check-true (approval-decide! id-b digest-b #t))
                      (define-values (outcome-a grant-a) (approval-await-grant id-a digest-a 20))
                      (define-values (outcome-b grant-b) (approval-await-grant id-b digest-b 20))
                      (check-equal? outcome-a 'denied)
                      (check-false grant-a)
                      (check-equal? outcome-b 'approved)
                      (check-true (consume-grant grant-b digest-b)))))

    (test-case "decision, await claim, and grant consumption are each exactly once"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (check-true (approval-decide! id digest-a #t))
                      (check-false (approval-decide! id digest-a #t))
                      (check-false (approval-decide! id digest-a #f))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-true (approval-grant? grant))
                      (define-values (replay-outcome replay-grant)
                        (approval-await-grant id digest-a 1))
                      (check-equal? replay-outcome 'cancelled)
                      (check-false replay-grant)
                      (check-false (consume-grant grant digest-b))
                      (check-true (consume-grant grant digest-a))
                      (check-false (consume-grant grant digest-a)))))

    (test-case "timeout removes correlation and rejects a late replay"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define-values (outcome grant) (approval-await-grant id digest-a 5))
                      (check-equal? outcome 'timed-out)
                      (check-false grant)
                      (check-false (approval-decide! id digest-a #t))
                      (check-equal? (pending-approval-count) 0))))

    (test-case "concurrent registrations produce unique correlated IDs"
      (with-channel (lambda (channel)
                      (define output (make-channel))
                      (define workers
                        (for/list ([_ (in-range 10)])
                          (thread (lambda ()
                                    (channel-put output
                                                 (register-approval-request-for-channel! channel
                                                                                         digest-a
                                                                                         view-a))))))
                      (define ids
                        (for/list ([_ (in-range 10)])
                          (channel-get output)))
                      (for-each thread-wait workers)
                      (check-equal? (length (remove-duplicates ids)) 10)
                      (check-equal? (pending-approval-count) 10)
                      (clear-pending-approvals!))))

    (test-case "production approval request uses an execution-plan commitment"
      (with-channel (lambda (_channel)
                      (define plan (make-test-plan))
                      (define requested (make-semaphore 0))
                      (define requested-payload (box #f))
                      (define publisher
                        (lambda (event-type payload)
                          (when (string=? event-type "mas.spawn-approval-requested")
                            (set-box! requested-payload payload)
                            (semaphore-post requested))))
                      (define decider
                        (thread (lambda ()
                                  (semaphore-wait requested)
                                  (define payload (unbox requested-payload))
                                  (approval-decide! (hash-ref payload 'request-id)
                                                    (hash-ref payload 'commitment-digest)
                                                    #t))))
                      (define approved
                        (request-spawn-approval plan (make-exec-context #:event-publisher publisher)))
                      (thread-wait decider)
                      (check-not-false approved)
                      (define payload (unbox requested-payload))
                      (check-equal? (hash-ref payload 'commitment-digest)
                                    (spawn-execution-plan-digest plan))
                      (check-true (string? (hash-ref payload 'request-id)))
                      (check-equal? (pending-approval-count) 0))))

    (test-case "publisher failure cancels its digest-bound request"
      (with-channel
       (lambda (_channel)
         (define plan (make-test-plan "blocked"))
         (define context
           (make-exec-context #:event-publisher
                              (lambda (event-type _payload)
                                (when (string=? event-type "mas.spawn-approval-requested")
                                  (error 'publisher "simulated failure")))))
         (check-exn #rx"simulated failure" (lambda () (request-spawn-approval plan context)))
         (check-equal? (pending-approval-count) 0))))

    (test-case "key handler decides using request ID and commitment digest"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define context
                        (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))
                      (set-box! (tui-ctx-ui-state-box context)
                                (struct-copy ui-state
                                             (initial-ui-state)
                                             [active-overlay
                                              (overlay-state 'approval-prompt
                                                             '()
                                                             ""
                                                             'top-left
                                                             #f
                                                             #f
                                                             0
                                                             (hasheq 'capabilities
                                                                     '(shell-exec)
                                                                     'task-preview
                                                                     "correlated test"
                                                                     'request-id
                                                                     id
                                                                     'commitment-digest
                                                                     digest-a))]))
                      (check-equal? (handle-approval-overlay-key context #\y) 'handled)
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-true (consume-grant grant digest-a)))))

    (test-case "key handler rejects a forged digest and leaves request pending"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define context
                        (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))
                      (set-box! (tui-ctx-ui-state-box context)
                                (struct-copy ui-state
                                             (initial-ui-state)
                                             [active-overlay
                                              (overlay-state
                                               'approval-prompt
                                               '()
                                               ""
                                               'top-left
                                               #f
                                               #f
                                               0
                                               (hasheq 'request-id id 'commitment-digest digest-b))]))
                      (check-equal? (handle-approval-overlay-key context #\y) 'handled)
                      (check-true (approval-request-pending? id digest-a))
                      (check-true (cancel-approval-request! id)))))))

(exit (run-tests suite))
