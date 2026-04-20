#lang racket/base

;; agent/wait-idle.rkt — FEAT-78: waitForIdle for SDK
;;
;; Provides a synchronization primitive that blocks until the agent
;; loop completes an iteration (idle state). Uses event bus subscription.
;; Lives in agent/ because it depends on agent/event-bus.

(require racket/contract
         "../util/protocol-types.rkt"
         "event-bus.rkt")

(provide (contract-out [wait-for-idle! (->* (event-bus? string?) ((or/c #f number?)) any)]))

;; ============================================================
;; wait-for-idle! : event-bus? string? (or/c #f number?) -> any
;;
;; Blocks until an iteration.completed or iteration.ended event
;; is observed for the given session-id.
;; Optional timeout in milliseconds ( #f = no timeout ).
;; Returns 'idle on success, 'timeout on timeout.
;; ============================================================

(define (wait-for-idle! bus session-id [timeout-ms #f])
  (define done-channel (make-channel))
  (define sub-id
    (subscribe! bus
                (lambda (evt)
                  (when (and (equal? (event-session-id evt) session-id)
                             (or (equal? (event-ev evt) "iteration.completed")
                                 (equal? (event-ev evt) "iteration.ended")))
                    (channel-put done-channel 'idle)))
                #:filter (lambda (evt)
                           (member (event-ev evt) '("iteration.completed" "iteration.ended")))))
  (define result
    (dynamic-wind void
                  (lambda ()
                    (if timeout-ms
                        (sync/timeout (/ timeout-ms 1000.0) done-channel)
                        (sync done-channel)))
                  (lambda () (unsubscribe! bus sub-id))))
  (or result 'timeout))
