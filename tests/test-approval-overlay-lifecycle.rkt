#lang racket/base

(require rackunit
         rackunit/text-ui
         "../tui/state-types.rkt"
         (only-in "../tui/state-events/core-handlers.rkt"
                  handle-spawn-approval-requested
                  handle-spawn-approval-terminal)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  register-approval-request!
                  cancel-approval-request!))

(define (approval-event id task)
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id id 'capabilities '(shell-exec) 'task-preview task)))

(define (terminal-event id)
  (make-event "mas.spawn-approval-terminal"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (if id
                  (hasheq 'request-id id)
                  (hasheq))))

(define (active-id state)
  (define ov (ui-state-active-overlay state))
  (and ov (hash-ref (overlay-state-extra ov) 'request-id #f)))

(define (queued-ids state)
  (define ov (ui-state-active-overlay state))
  (if ov
      (map (lambda (request) (hash-ref request 'request-id #f))
           (hash-ref (overlay-state-extra ov) 'approval-queue '()))
      '()))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define-syntax-rule (approval-test-case name body ...)
  (test-case name
    (with-approval-channel (lambda ()
                             body ...))))

(define suite
  (test-suite "approval overlay lifecycle"

    (approval-test-case "requests are displayed and promoted in FIFO order"
                        (define id-a (register-approval-request!))
                        (define id-b (register-approval-request!))
                        (define s0 (initial-ui-state))
                        (define s1 (handle-spawn-approval-requested s0 (approval-event id-a "first")))
                        (define s2
                          (handle-spawn-approval-requested s1 (approval-event id-b "second")))
                        (check-equal? (active-id s2) id-a)
                        (check-equal? (queued-ids s2) (list id-b))
                        (define s3 (handle-spawn-approval-terminal s2 (terminal-event id-a)))
                        (check-equal? (active-id s3) id-b)
                        (check-equal? (queued-ids s3) '()))

    (approval-test-case
     "stale duplicate request cannot replace the promoted request"
     (define id-a (register-approval-request!))
     (define id-b (register-approval-request!))
     (define s1 (handle-spawn-approval-requested (initial-ui-state) (approval-event id-a "first")))
     (define s2 (handle-spawn-approval-requested s1 (approval-event id-b "second")))
     (define s3 (handle-spawn-approval-terminal s2 (terminal-event id-a)))
     (define after-stale-terminal (handle-spawn-approval-terminal s3 (terminal-event id-a)))
     (check-equal? (active-id after-stale-terminal) id-b)
     (check-true (cancel-approval-request! id-a))
     (define s4
       (handle-spawn-approval-requested after-stale-terminal (approval-event id-a "duplicate")))
     (check-equal? (active-id s4) id-b)
     (check-equal? (queued-ids s4) '()))

    (approval-test-case
     "terminal-before-request delivery is rejected"
     (define stale-id (register-approval-request!))
     (define after-terminal
       (handle-spawn-approval-terminal (initial-ui-state) (terminal-event stale-id)))
     (check-true (cancel-approval-request! stale-id))
     (define state
       (handle-spawn-approval-requested after-terminal (approval-event stale-id "already terminal")))
     (check-false (ui-state-active-overlay state)))

    (approval-test-case
     "terminal event removes only a matching queued request"
     (define id-a (register-approval-request!))
     (define id-b (register-approval-request!))
     (define id-c (register-approval-request!))
     (define s1 (handle-spawn-approval-requested (initial-ui-state) (approval-event id-a "first")))
     (define s2 (handle-spawn-approval-requested s1 (approval-event id-b "second")))
     (define s3 (handle-spawn-approval-requested s2 (approval-event id-c "third")))
     (define s4 (handle-spawn-approval-terminal s3 (terminal-event id-b)))
     (check-equal? (active-id s4) id-a)
     (check-equal? (queued-ids s4) (list id-c)))

    (approval-test-case
     "uncorrelated terminal event is ignored"
     (define id (register-approval-request!))
     (define stale-id (register-approval-request!))
     (check-true (cancel-approval-request! stale-id))
     (define state (handle-spawn-approval-requested (initial-ui-state) (approval-event id "first")))
     (check-eq? (handle-spawn-approval-terminal state (terminal-event #f)) state)
     (check-eq? (handle-spawn-approval-terminal state (terminal-event stale-id)) state))))

(run-tests suite)
