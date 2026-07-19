#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         rackunit/text-ui
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  tui-ctx-ui-state-box
                  handle-approval-overlay-key)
         "../tui/state-types.rkt"
         (only-in "../tui/state-events/core-handlers.rkt" handle-spawn-approval-requested)
         (only-in "../tui/message-dispatch.rkt" process-tui-message!)
         (only-in "../tools/builtins/spawn-subagent.rkt" request-spawn-approval)
         (only-in "../tools/builtins/spawn-execution-plan.rkt" make-spawn-execution-plan)
         (only-in "../tools/tool.rkt" make-exec-context)
         (only-in "../tui/context.rkt" tui-ctx-input-state-box)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-request-pending?
                  approval-decide!
                  approval-await-grant
                  approval-grant?
                  call-with-approval-grant
                  cancel-approval-request!
                  pending-approval-count))

(define DIGEST-A (make-string 64 #\a))
(define DIGEST-B (make-string 64 #\b))
(define PRESENTATION-A (make-string 64 #\c))
(define PRESENTATION-B (make-string 64 #\d))

(struct approval-fixture (id commitment-digest presentation-digest) #:transparent)

(define (make-test-ctx)
  (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))

(define (register-fixture task
                          #:commitment-digest [commitment-digest DIGEST-A]
                          #:presentation-digest [presentation-digest PRESENTATION-A])
  (define view
    (hasheq 'capabilities '(shell-exec) 'task-preview task 'presentation-digest presentation-digest))
  (define id
    (register-approval-request-for-channel! (current-approval-channel) commitment-digest view))
  (unless id
    (error 'register-fixture "active broker registration failed"))
  (approval-fixture id commitment-digest presentation-digest))

(define (approval-event fixture)
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id
                      (approval-fixture-id fixture)
                      'commitment-digest
                      (approval-fixture-commitment-digest fixture)
                      'presentation-digest
                      (approval-fixture-presentation-digest fixture))))

(define (install-requests! ctx fixtures)
  (define state
    (for/fold ([state (initial-ui-state)]) ([fixture (in-list fixtures)])
      (handle-spawn-approval-requested state (approval-event fixture))))
  (set-box! (tui-ctx-ui-state-box ctx) state))

(define (active-id ctx)
  (define overlay (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
  (and overlay (hash-ref (overlay-state-extra overlay) 'request-id #f)))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define (check-delivery fixture expected)
  (define-values (outcome grant)
    (approval-await-grant (approval-fixture-id fixture)
                          (approval-fixture-commitment-digest fixture)
                          50))
  (check-equal? outcome (if expected 'approved 'denied))
  (cond
    [expected
     (check-true (approval-grant? grant))
     (check-true
      (call-with-approval-grant grant (approval-fixture-commitment-digest fixture) (lambda () #t)))]
    [else (check-false grant)]))

(define suite
  (test-suite "TUI digest-bound approval overlay key handler"

    (test-case "y variants approve the exact commitment and dismiss"
      (for ([key (in-list (list #\y #\Y 'y))])
        (with-approval-channel (lambda ()
                                 (define fixture (register-fixture "approve"))
                                 (define ctx (make-test-ctx))
                                 (install-requests! ctx (list fixture))
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-delivery fixture #t)
                                 (check-false (ui-state-active-overlay
                                               (unbox (tui-ctx-ui-state-box ctx))))))))

    (test-case "n variants and Escape deny the exact commitment and dismiss"
      (for ([key (in-list (list #\n #\N 'n 'escape))])
        (with-approval-channel (lambda ()
                                 (define fixture (register-fixture "deny"))
                                 (define ctx (make-test-ctx))
                                 (install-requests! ctx (list fixture))
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-delivery fixture #f)
                                 (check-false (ui-state-active-overlay
                                               (unbox (tui-ctx-ui-state-box ctx))))))))

    (test-case "successful answer promotes the next FIFO digest-bound request"
      (with-approval-channel
       (lambda ()
         (define fixture-a
           (register-fixture "A" #:commitment-digest DIGEST-A #:presentation-digest PRESENTATION-A))
         (define fixture-b
           (register-fixture "B" #:commitment-digest DIGEST-B #:presentation-digest PRESENTATION-B))
         (define ctx (make-test-ctx))
         (install-requests! ctx (list fixture-a fixture-b))
         (check-equal? (active-id ctx) (approval-fixture-id fixture-a))
         (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)
         (check-delivery fixture-a #t)
         (check-equal? (active-id ctx) (approval-fixture-id fixture-b))
         (define promoted-extra
           (overlay-state-extra (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx)))))
         (check-equal? (hash-ref promoted-extra 'commitment-digest) DIGEST-B)
         (check-equal? (hash-ref promoted-extra 'presentation-digest) PRESENTATION-B))))

    (test-case "wrong-digest overlay cannot approve the broker commitment"
      (with-approval-channel
       (lambda ()
         (define fixture (register-fixture "must remain pending"))
         (define ctx (make-test-ctx))
         (install-requests! ctx (list fixture))
         (define state (unbox (tui-ctx-ui-state-box ctx)))
         (define overlay (ui-state-active-overlay state))
         (define forged-extra (hash-set (overlay-state-extra overlay) 'commitment-digest DIGEST-B))
         (set-box! (tui-ctx-ui-state-box ctx)
                   (struct-copy ui-state
                                state
                                [active-overlay
                                 (struct-copy overlay-state overlay [extra forged-extra])]))
         (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)
         (check-true (approval-request-pending? (approval-fixture-id fixture)
                                                (approval-fixture-commitment-digest fixture)))
         (check-false (approval-decide! (approval-fixture-id fixture) DIGEST-B #t))
         (check-true (cancel-approval-request! (approval-fixture-id fixture))))))

    (test-case "stale terminal request is pruned without deciding the queued request"
      (with-approval-channel
       (lambda ()
         (define stale-fixture
           (register-fixture "stale"
                             #:commitment-digest DIGEST-A
                             #:presentation-digest PRESENTATION-A))
         (define current-fixture
           (register-fixture "current"
                             #:commitment-digest DIGEST-B
                             #:presentation-digest PRESENTATION-B))
         (define ctx (make-test-ctx))
         (install-requests! ctx (list stale-fixture current-fixture))
         (check-true (approval-decide! (approval-fixture-id stale-fixture)
                                       (approval-fixture-commitment-digest stale-fixture)
                                       #t))
         (check-equal? (handle-approval-overlay-key ctx #\n) 'handled)
         (check-equal? (active-id ctx) (approval-fixture-id current-fixture))
         (check-true (approval-request-pending? (approval-fixture-id current-fixture)
                                                (approval-fixture-commitment-digest current-fixture)))
         (check-delivery stale-fixture #t))))

    (test-case "missing commitment digest cannot deliver or dismiss"
      (with-approval-channel
       (lambda ()
         (define fixture (register-fixture "correlated only by id"))
         (define ctx (make-test-ctx))
         (set-box! (tui-ctx-ui-state-box ctx)
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
                                                (hasheq 'request-id
                                                        (approval-fixture-id fixture)
                                                        'capabilities
                                                        '(shell-exec)
                                                        'task-preview
                                                        "missing digest"))]))
         (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)
         (check-not-false (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
         (check-true (approval-request-pending? (approval-fixture-id fixture)
                                                (approval-fixture-commitment-digest fixture)))
         (check-true (cancel-approval-request! (approval-fixture-id fixture))))))

    (test-case "all non-decision keys are consumed while modal"
      (with-approval-channel (lambda ()
                               (define fixture (register-fixture "modal"))
                               (define ctx (make-test-ctx))
                               (install-requests! ctx (list fixture))
                               (for ([key (in-list (list #\a 'return 'up 'tab))])
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-equal? (active-id ctx) (approval-fixture-id fixture))))))

    (test-case "killed approval owner is cancelled and its modal is pruned"
      (with-approval-channel
       (lambda ()
         (define ctx (make-test-ctx))
         (define shown (make-semaphore 0))
         (define publisher
           (lambda (type payload)
             (when (string=? type "mas.spawn-approval-requested")
               (define event
                 (make-event type (current-inexact-milliseconds) "test-session" #f payload))
               (set-box! (tui-ctx-ui-state-box ctx)
                         (handle-spawn-approval-requested (unbox (tui-ctx-ui-state-box ctx)) event))
               (semaphore-post shown))))
         (define plan
           (make-spawn-execution-plan 'single
                                      (hasheq 'task "blocked" 'capabilities '(shell-exec))
                                      (hasheq 'task-preview "blocked" 'capabilities '(shell-exec))))
         (define owner
           (thread (lambda ()
                     (request-spawn-approval plan (make-exec-context #:event-publisher publisher)))))
         (check-not-false (sync/timeout 1 shown))
         (check-not-false (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
         (kill-thread owner)
         (thread-wait owner)
         (let wait ([attempts 100])
           (when (and (positive? attempts) (positive? (pending-approval-count)))
             (sleep 0.01)
             (wait (sub1 attempts))))
         (check-equal? (pending-approval-count) 0)
         (process-tui-message! ctx '(redraw))
         (check-false (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx)))))))

    (test-case "paste and mouse are consumed while approval is modal"
      (with-approval-channel (lambda ()
                               (define fixture (register-fixture "modal input"))
                               (define ctx (make-test-ctx))
                               (install-requests! ctx (list fixture))
                               (define input-before (unbox (tui-ctx-input-state-box ctx)))
                               (define ui-before (unbox (tui-ctx-ui-state-box ctx)))
                               (process-tui-message! ctx '(paste "must-not-insert"))
                               (process-tui-message! ctx '(mouse click 0 1 1))
                               (check-equal? (unbox (tui-ctx-input-state-box ctx)) input-before)
                               (check-eq? (unbox (tui-ctx-ui-state-box ctx)) ui-before))))

    (test-case "non-approval overlays still pass"
      (define ctx (make-test-ctx))
      (check-equal? (handle-approval-overlay-key ctx #\y) 'pass)
      (set-box! (tui-ctx-ui-state-box ctx)
                (struct-copy ui-state
                             (initial-ui-state)
                             [active-overlay
                              (overlay-state 'tree-browser '() "" 'top-left #f #f 0 #f)]))
      (check-equal? (handle-approval-overlay-key ctx #\y) 'pass))))

(run-tests suite)
