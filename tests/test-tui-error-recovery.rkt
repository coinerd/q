#lang racket

;; tests/test-tui-error-recovery.rkt — P0: TUI recovers after runner thread exception
;; Verifies that exception in run-prompt! does not hang TUI (busy? cleared,
;; error displayed, subsequent prompts work)

(require rackunit
         rackunit/text-ui
         "../tui/state.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt")

(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [sid "test-session"]
                         #:turn-id [tid "turn-1"])
  (event 1 ev-type time sid tid payload))

;; Simulate the TUI error recovery sequence:
;; 1. turn.started → busy?=#t
;; 2. runtime.error with internal-error → busy?=#f, error displayed
;; 3. turn.completed → busy?=#f (idempotent)

(define-test-suite
 test-tui-error-recovery
 (test-case "error recovery: runtime.error clears busy?"
   (define s0 (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
   (define evt
     (make-test-event "runtime.error" (hasheq 'error "test error" 'errorType 'internal-error)))
   (define s1 (apply-event-to-state s0 evt))
   (check-false (ui-state-busy? s1) "busy? cleared after runtime.error"))
 (test-case "error recovery: error message shown in transcript"
   (define s0 (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
   (define evt
     (make-test-event "runtime.error"
                      (hasheq 'error "something went wrong" 'errorType 'internal-error)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "error adds transcript entry")
   (define entry (last entries))
   (check-equal? (transcript-entry-kind entry) 'error))
 (test-case "error recovery: turn.completed after error is idempotent"
   (define s0 (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
   (define err-evt
     (make-test-event "runtime.error" (hasheq 'error "test error" 'errorType 'internal-error)))
   (define s1 (apply-event-to-state s0 err-evt))
   (check-false (ui-state-busy? s1) "busy? cleared after error")
   ;; turn.completed should keep busy?=#f
   (define done-evt (make-test-event "turn.completed" (hasheq 'reason "error")))
   (define s2 (apply-event-to-state s1 done-evt))
   (check-false (ui-state-busy? s2) "busy? still false after turn.completed"))
 (test-case "error recovery: subsequent prompt works after error"
   (define s0 (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
   (define err-evt
     (make-test-event "runtime.error" (hasheq 'error "test error" 'errorType 'internal-error)))
   (define s1 (apply-event-to-state s0 err-evt))
   ;; Now simulate a new turn
   (define start-evt (make-test-event "turn.started" (hasheq)))
   (define s2 (apply-event-to-state s1 start-evt))
   (check-true (ui-state-busy? s2) "new turn sets busy?=#t after error recovery"))
 (test-case "error recovery: internal-error hint shown"
   (define s0 (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
   (define evt
     (make-test-event "runtime.error" (hasheq 'error "internal failure" 'errorType 'internal-error)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (define system-entries (filter (lambda (e) (eq? (transcript-entry-kind e) 'system)) entries))
   (check > (length system-entries) 0 "system entry present")
   (define hint-text (transcript-entry-text (last system-entries)))
   (check-true (string-contains? (string-downcase hint-text) "internal")
               (format "hint mentions internal: ~a" hint-text)))
 (test-case "error recovery: streaming state cleared on error"
   (define s0
     (struct-copy ui-state
                  (initial-ui-state #:session-id "s1")
                  [busy? #t]
                  [streaming-text "partial code..."]
                  [streaming-thinking "thinking..."]))
   (define evt (make-test-event "runtime.error" (hasheq 'error "timeout" 'errorType 'timeout)))
   (define s1 (apply-event-to-state s0 evt))
   (check-false (ui-state-streaming-text s1) "streaming-text cleared")
   (check-false (ui-state-streaming-thinking s1) "streaming-thinking cleared")))

(run-test test-tui-error-recovery)
