#lang racket/base

;; runtime/iteration/loop-phases.rkt — extracted pure and effectful sub-phases
;; STABILITY: internal
;;
;; Extracted from main-loop.rkt to separate concerns:
;;   - prepare-iteration-context: pure context assembly (steering + injected)
;;   - dispatch-turn-start-hooks: effectful hook dispatch with block detection
;;
;; These functions make the iteration loop's phases testable in isolation.

(require racket/contract
         racket/list
         (only-in "../../agent/event-bus.rkt" event-bus? publish!)
         (only-in "../../util/event-contracts.rkt" injection-count-payload/c)
         (only-in "tool-turn-bridge.rkt" dequeue-all-steering! drain-injected-messages!)
         (only-in "loop-config.rkt"
                  loop-config-queue
                  loop-config-injected-box
                  loop-config-bus
                  loop-config-session-id
                  loop-config?)
         (only-in "../../agent/queue.rkt" queue? queue-status)
         (only-in "../../runtime/runtime-helpers.rkt" maybe-dispatch-hooks emit-session-event!)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../../extensions/api.rkt" extension-registry?)
         (only-in "internal.rkt" assert-payload))

;; Re-export for consumers
(provide (contract-out [prepare-iteration-context
                        (-> list?
                            (or/c queue? #f)
                            (or/c box? #f)
                            event-bus?
                            (or/c extension-registry? #f)
                            string?
                            list?)]
                       [dispatch-turn-start-hooks
                        (-> list? (or/c extension-registry? #f) (values list? boolean?))]))

;; ============================================================
;; Pure-ish: Context preparation
;; ============================================================

;; Prepare iteration context by dequeuing steering and injected messages.
;; Returns the augmented context list.
(define (prepare-iteration-context ctx steering-queue injected-box bus ext-reg session-id)
  ;; Steering messages
  (define ctx-with-steering
    (if steering-queue
        (let ([steering-msgs (dequeue-all-steering! steering-queue)])
          (let ([qs (queue-status steering-queue)])
            (when (or (> (hash-ref qs 'steering 0) 0) (> (hash-ref qs 'followup 0) 0))
              (emit-session-event! bus session-id "queue.status-update" qs)))
          (if (null? steering-msgs)
              ctx
              (append ctx steering-msgs)))
        ctx))
  ;; Injected messages
  (if injected-box
      (let ([injected (drain-injected-messages! bus injected-box session-id)])
        (if (null? injected)
            ctx-with-steering
            (begin
              (emit-session-event! bus
                                   session-id
                                   "message.injected.drain"
                                   (assert-payload "message.injected.drain"
                                                   (hasheq 'count (length injected))
                                                   injection-count-payload/c))
              (append ctx-with-steering injected))))
      ctx-with-steering))

;; ============================================================
;; Effectful: Turn-start hook dispatch
;; ============================================================

;; Dispatch turn-start hooks. Returns (values amended-ctx blocked?).
(define (dispatch-turn-start-hooks ctx ext-reg)
  (let-values ([(amended-ctx hook-res) (maybe-dispatch-hooks ext-reg 'turn-start ctx)])
    (if (and hook-res (eq? (hook-result-action hook-res) 'block))
        (values ctx #t)
        (values amended-ctx #f))))
