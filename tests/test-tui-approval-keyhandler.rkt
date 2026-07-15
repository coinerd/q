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
         (only-in "../tools/tool.rkt" make-exec-context)
         (only-in "../tui/context.rkt" tui-ctx-input-state-box)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  approval-channel-ch
                  register-approval-request!
                  pending-approval-count
                  approval-await-for-id
                  approval-put-for-id!))

(define (make-test-ctx)
  (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))

(define (approval-event id task)
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id id 'capabilities '(shell-exec) 'task-preview task)))

(define (install-requests! ctx ids)
  (define state
    (for/fold ([state (initial-ui-state)]) ([id (in-list ids)])
      (handle-spawn-approval-requested state (approval-event id id))))
  (set-box! (tui-ctx-ui-state-box ctx) state))

(define (active-id ctx)
  (define ov (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
  (and ov (hash-ref (overlay-state-extra ov) 'request-id #f)))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define (check-delivery id expected)
  (define-values (approved? delivered?) (approval-await-for-id id 50))
  (check-true delivered?)
  (check-equal? approved? expected))

(define suite
  (test-suite "TUI approval overlay key handler"

    (test-case "y variants approve and dismiss"
      (for ([key (in-list (list #\y #\Y 'y))])
        (with-approval-channel (lambda ()
                                 (define id (register-approval-request!))
                                 (define ctx (make-test-ctx))
                                 (install-requests! ctx (list id))
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-delivery id #t)
                                 (check-false (ui-state-active-overlay
                                               (unbox (tui-ctx-ui-state-box ctx))))))))

    (test-case "n variants and Escape deny and dismiss"
      (for ([key (in-list (list #\n #\N 'n 'escape))])
        (with-approval-channel (lambda ()
                                 (define id (register-approval-request!))
                                 (define ctx (make-test-ctx))
                                 (install-requests! ctx (list id))
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-delivery id #f)
                                 (check-false (ui-state-active-overlay
                                               (unbox (tui-ctx-ui-state-box ctx))))))))

    (test-case "successful answer promotes the next FIFO request"
      (with-approval-channel (lambda ()
                               (define id-a (register-approval-request!))
                               (define id-b (register-approval-request!))
                               (define ctx (make-test-ctx))
                               (install-requests! ctx (list id-a id-b))
                               (check-equal? (active-id ctx) id-a)
                               (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)
                               (check-delivery id-a #t)
                               (check-equal? (active-id ctx) id-b))))

    (test-case "stale terminal request is removed without dismissing the queued current request"
      (with-approval-channel
       (lambda ()
         (define stale-id (register-approval-request!))
         (define current-id (register-approval-request!))
         (define ctx (make-test-ctx))
         ;; Create the overlay while both IDs are pending, then make its active
         ;; request terminal so the attempted key delivery must fail.
         (install-requests! ctx (list stale-id current-id))
         (check-true (approval-put-for-id! stale-id #t))
         (check-equal? (handle-approval-overlay-key ctx #\n) 'handled)
         (check-equal? (active-id ctx) current-id)
         (check-equal? (sync/timeout 0 (approval-channel-ch (current-approval-channel))) #f))))

    (test-case "missing request-id never falls back to the legacy channel"
      (with-approval-channel
       (lambda ()
         (define ctx (make-test-ctx))
         (set-box! (tui-ctx-ui-state-box ctx)
                   (struct-copy
                    ui-state
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
                      (hasheq 'capabilities '(shell-exec) 'task-preview "uncorrelated"))]))
         (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)
         (check-not-false (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
         (check-false (sync/timeout 0 (approval-channel-ch (current-approval-channel)))))))

    (test-case "all non-decision keys are consumed while modal"
      (with-approval-channel (lambda ()
                               (define id (register-approval-request!))
                               (define ctx (make-test-ctx))
                               (install-requests! ctx (list id))
                               (for ([key (in-list (list #\a 'return 'up 'tab))])
                                 (check-equal? (handle-approval-overlay-key ctx key) 'handled)
                                 (check-equal? (active-id ctx) id)))))

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
         (define owner
           (thread (lambda ()
                     (request-spawn-approval '(shell-exec)
                                             "blocked"
                                             (make-exec-context #:event-publisher publisher)))))
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
                               (define id (register-approval-request!))
                               (define ctx (make-test-ctx))
                               (install-requests! ctx (list id))
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
