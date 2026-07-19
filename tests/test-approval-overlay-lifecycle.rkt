#lang racket/base

(require rackunit
         rackunit/text-ui
         "../tui/state-types.rkt"
         (only-in "../tui/state-events/core-handlers.rkt"
                  handle-spawn-approval-requested
                  handle-spawn-approval-terminal)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-decide!
                  cancel-approval-request!))

(define DIGEST-A (make-string 64 #\a))
(define DIGEST-B (make-string 64 #\b))
(define DIGEST-C (make-string 64 #\c))
(define DIGEST-WRONG (make-string 64 #\f))
(define PRESENTATION-A (make-string 64 #\1))
(define PRESENTATION-B (make-string 64 #\2))
(define PRESENTATION-C (make-string 64 #\3))

(struct approval-fixture (id commitment-digest presentation-digest) #:transparent)

(define (register-fixture task commitment-digest presentation-digest [capabilities '(shell-exec)])
  (define authoritative-view
    (hasheq 'capabilities capabilities 'task-preview task 'presentation-digest presentation-digest))
  (define id
    (register-approval-request-for-channel! (current-approval-channel)
                                            commitment-digest
                                            authoritative-view))
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

(define (terminal-event fixture)
  (make-event "mas.spawn-approval-terminal"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (if fixture
                  (hasheq 'request-id
                          (approval-fixture-id fixture)
                          'commitment-digest
                          (approval-fixture-commitment-digest fixture))
                  (hasheq))))

(define (terminalize-event fixture)
  (check-true
   (approval-decide! (approval-fixture-id fixture) (approval-fixture-commitment-digest fixture) #f))
  (terminal-event fixture))

(define (active-extra state)
  (define overlay (ui-state-active-overlay state))
  (and overlay (overlay-state-extra overlay)))

(define (active-id state)
  (define extra (active-extra state))
  (and extra (hash-ref extra 'request-id #f)))

(define (queued-requests state)
  (define extra (active-extra state))
  (if extra
      (hash-ref extra 'approval-queue '())
      '()))

(define (queued-ids state)
  (map (lambda (request) (hash-ref request 'request-id #f)) (queued-requests state)))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define-syntax-rule (approval-test-case name body ...)
  (test-case name
    (with-approval-channel (lambda ()
                             body ...))))

(define suite
  (test-suite "digest-bound approval overlay lifecycle"

    (approval-test-case
     "requests are displayed and promoted in FIFO order with digest bindings"
     (define fixture-a (register-fixture "first" DIGEST-A PRESENTATION-A))
     (define fixture-b (register-fixture "second" DIGEST-B PRESENTATION-B '(file-write)))
     (define s1 (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture-a)))
     (define s2 (handle-spawn-approval-requested s1 (approval-event fixture-b)))
     (check-equal? (active-id s2) (approval-fixture-id fixture-a))
     (check-equal? (hash-ref (active-extra s2) 'commitment-digest) DIGEST-A)
     (check-equal? (hash-ref (active-extra s2) 'presentation-digest) PRESENTATION-A)
     (check-equal? (queued-ids s2) (list (approval-fixture-id fixture-b)))
     (check-equal? (hash-ref (car (queued-requests s2)) 'commitment-digest) DIGEST-B)
     (define s3 (handle-spawn-approval-terminal s2 (terminalize-event fixture-a)))
     (check-equal? (active-id s3) (approval-fixture-id fixture-b))
     (check-equal? (hash-ref (active-extra s3) 'commitment-digest) DIGEST-B)
     (check-equal? (hash-ref (active-extra s3) 'presentation-digest) PRESENTATION-B)
     (check-equal? (queued-ids s3) '()))

    (approval-test-case
     "wrong-digest request cannot alter the active lifecycle"
     (define fixture-a (register-fixture "first" DIGEST-A PRESENTATION-A))
     (define fixture-b (register-fixture "second" DIGEST-B PRESENTATION-B))
     (define state (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture-a)))
     (define wrong-event
       (make-event "mas.spawn-approval-requested"
                   (current-inexact-milliseconds)
                   "test-session"
                   #f
                   (hasheq 'request-id
                           (approval-fixture-id fixture-b)
                           'commitment-digest
                           DIGEST-WRONG
                           'presentation-digest
                           PRESENTATION-B)))
     (define rejected (handle-spawn-approval-requested state wrong-event))
     (check-eq? rejected state)
     (check-equal? (active-id rejected) (approval-fixture-id fixture-a))
     (check-equal? (queued-ids rejected) '()))

    (approval-test-case
     "stale duplicate request cannot replace the promoted request"
     (define fixture-a (register-fixture "first" DIGEST-A PRESENTATION-A))
     (define fixture-b (register-fixture "second" DIGEST-B PRESENTATION-B))
     (define s1 (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture-a)))
     (define s2 (handle-spawn-approval-requested s1 (approval-event fixture-b)))
     (define s3 (handle-spawn-approval-terminal s2 (terminalize-event fixture-a)))
     (define after-stale-terminal (handle-spawn-approval-terminal s3 (terminal-event fixture-a)))
     (check-equal? (active-id after-stale-terminal) (approval-fixture-id fixture-b))
     (check-true (cancel-approval-request! (approval-fixture-id fixture-a)))
     (define s4 (handle-spawn-approval-requested after-stale-terminal (approval-event fixture-a)))
     (check-equal? (active-id s4) (approval-fixture-id fixture-b))
     (check-equal? (queued-ids s4) '()))

    (approval-test-case
     "forged terminal cannot dismiss a live or wrong-digest request"
     (define fixture (register-fixture "live" DIGEST-A PRESENTATION-A))
     (define state (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture)))
     (check-eq? (handle-spawn-approval-terminal state (terminal-event fixture)) state)
     (define wrong-terminal
       (make-event
        "mas.spawn-approval-terminal"
        (current-inexact-milliseconds)
        "test-session"
        #f
        (hasheq 'request-id (approval-fixture-id fixture) 'commitment-digest DIGEST-WRONG)))
     (check-eq? (handle-spawn-approval-terminal state wrong-terminal) state)
     (check-equal? (active-id state) (approval-fixture-id fixture)))

    (approval-test-case
     "terminal event removes only a matching queued request"
     (define fixture-a (register-fixture "first" DIGEST-A PRESENTATION-A))
     (define fixture-b (register-fixture "second" DIGEST-B PRESENTATION-B))
     (define fixture-c (register-fixture "third" DIGEST-C PRESENTATION-C))
     (define s1 (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture-a)))
     (define s2 (handle-spawn-approval-requested s1 (approval-event fixture-b)))
     (define s3 (handle-spawn-approval-requested s2 (approval-event fixture-c)))
     (define s4 (handle-spawn-approval-terminal s3 (terminalize-event fixture-b)))
     (check-equal? (active-id s4) (approval-fixture-id fixture-a))
     (check-equal? (queued-ids s4) (list (approval-fixture-id fixture-c))))

    (approval-test-case
     "uncorrelated terminal event is ignored"
     (define fixture (register-fixture "first" DIGEST-A PRESENTATION-A))
     (define stale-fixture (register-fixture "stale" DIGEST-B PRESENTATION-B))
     (check-true (cancel-approval-request! (approval-fixture-id stale-fixture)))
     (define state (handle-spawn-approval-requested (initial-ui-state) (approval-event fixture)))
     (check-eq? (handle-spawn-approval-terminal state (terminal-event #f)) state)
     (check-eq? (handle-spawn-approval-terminal state (terminal-event stale-fixture)) state))))

(run-tests suite)
