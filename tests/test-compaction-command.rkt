#lang racket

;; @speed fast  ;; @suite runtime

;;; tests/test-compaction-command.rkt — /compact command tests

(require rackunit
         rackunit/text-ui
         "../tui/commands/runtime-control.rkt"
         "../tui/commands/context.rkt"
         "../tui/state.rkt"
         "../util/event/event-bus.rkt"
         "../util/message/protocol-types.rkt")

(define (make-test-cctx [bus #f])
  (cmd-ctx (box (initial-ui-state))
           (box #t)
           bus
           #f
           (box #f)
           #f
           (box #f)
           #f
           (box "")
           #f
           #f
           (box #f)
           (box #f)))

(define compact-tests
  (test-suite "/compact command"
    (test-case "/compact accepts once, enters in-progress state, and requests durable compaction"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
      (define cctx (make-test-cctx bus))
      (define result (handle-compact-command cctx (unbox (cmd-ctx-state-box cctx)) '()))
      (check-equal? result 'continue)
      (define evts (reverse (unbox events)))
      (check-equal? (length evts) 1)
      (check-equal? (event-ev (car evts)) "session.compact.requested")
      (check-true (hash-ref (event-payload (car evts)) 'persist? #f))
      (check-pred string? (hash-ref (event-payload (car evts)) 'request-id #f))
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (check-equal? (ui-state-status-message new-state) "Compacting...")
      (check-false (for/or ([entry (in-list (ui-state-transcript new-state))])
                     (string-contains? (transcript-entry-text entry) "compact requested"))))

    (test-case "/compact --dry-run refuses a misleading transcript preview"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
      (define cctx (make-test-cctx bus))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define state-with-entries
        (struct-copy ui-state
                     state
                     [transcript
                      (list (make-entry 'system "msg1" 0 (hash))
                            (make-entry 'system "msg2" 0 (hash))
                            (make-entry 'system "msg3" 0 (hash)))]))
      (set-box! (cmd-ctx-state-box cctx) state-with-entries)
      (define result (handle-compact-command cctx state-with-entries '("--dry-run")))
      (check-equal? result 'continue)
      (check-equal? (length (unbox events)) 0 "no event should be emitted for dry-run")
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "dry-run unavailable")))
      (check-false (for/or ([e (in-list transcript)])
                     (string-contains? (transcript-entry-text e) "would be compacted"))))

    (test-case "/compact without runtime bus fails visibly and never enters in-progress state"
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (check-equal? (handle-compact-command cctx state '()) 'continue)
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (check-false (ui-state-status-message new-state))
      (check-true (for/or ([entry (in-list (ui-state-transcript new-state))])
                    (string-contains? (transcript-entry-text entry) "event bus unavailable"))))

    (test-case "/compact blocked when busy"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
      (define cctx (make-test-cctx bus))
      (define state (set-busy (initial-ui-state) #t))
      (set-box! (cmd-ctx-state-box cctx) state)
      (define result (handle-compact-command cctx state '()))
      (check-equal? result 'continue)
      (check-equal? (length (unbox events)) 0 "no event should be emitted when busy")
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "active turn"))))))

(module+ main
  (run-tests compact-tests))
