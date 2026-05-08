#lang racket/base

;; runtime/iteration/counters.rkt — iteration counter logic + cancellation check
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:
;;   compute-next-counters — pure counter update after tool execution
;;   check-cancellation    — evaluate shutdown/cancellation conditions

(require racket/contract
         racket/list
         (only-in "loop-state.rkt"
                  loop-counters
                  loop-counters-seen-paths
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count
                  loop-counters-implement-count
                  loop-counters-consecutive-error-count
                  loop-counters-recent-tool-names)
         (only-in "../../util/protocol-types.rkt"
                  message-content
                  tool-result-part?
                  tool-result-part-is-error?
                  tool-call-name)
         (only-in "../../runtime/tool-coordinator.rkt"
                  extract-tool-calls-from-messages)
         (only-in "tool-turn-bridge.rkt"
                  update-seen-paths
                  take-at-most)
         (only-in "../../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../../agent/event-structs/hook-events.rkt" turn-cancelled-event)
         (only-in "../../util/loop-result.rkt" make-loop-result)
         (only-in "../../util/cancellation.rkt"
                  cancellation-token?
                  cancellation-token-cancelled?))

(provide compute-next-counters check-cancellation)

;; ============================================================
;; compute-next-counters
;; ============================================================

(define (compute-next-counters counters new-msgs)
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  (define-values (new-seen-paths should-increment?)
    (update-seen-paths current-tool-calls (loop-counters-seen-paths counters)))
  (define effective-tool-count
    (if should-increment?
        (add1 (loop-counters-consecutive-tool-count counters))
        (loop-counters-consecutive-tool-count counters)))
  (define new-explore-count
    (+ (loop-counters-explore-count counters)
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
  (define new-implement-count
    (+ (loop-counters-implement-count counters)
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
  (define new-error-count
    (+ (loop-counters-consecutive-error-count counters)
       (for/sum ([tr (filter tool-result-part? (apply append (map message-content new-msgs)))])
                (if (tool-result-part-is-error? tr) 1 0))))
  (define new-recent-tools
    (take-at-most (append (loop-counters-recent-tool-names counters)
                          (filter string? (map tool-call-name current-tool-calls)))
                  20))
  (struct-copy loop-counters
               counters
               [seen-paths new-seen-paths]
               [consecutive-tool-count effective-tool-count]
               [explore-count new-explore-count]
               [implement-count new-implement-count]
               [consecutive-error-count new-error-count]
               [recent-tool-names new-recent-tools]))

;; ============================================================
;; check-cancellation
;; ============================================================

(define (check-cancellation token force-shutdown-check shutdown-check bus session-id iteration ctx)
  (cond
    [(and force-shutdown-check (force-shutdown-check))
     (emit-typed-event! bus
                        (turn-cancelled-event "turn.cancelled"
                                              (current-inexact-milliseconds)
                                              session-id
                                              #f
                                              "force-shutdown"
                                              iteration))
     (make-loop-result ctx 'cancelled (hasheq 'reason "force-shutdown" 'iteration iteration))]
    [(and token (cancellation-token-cancelled? token))
     (emit-typed-event! bus
                        (turn-cancelled-event "turn.cancelled"
                                              (current-inexact-milliseconds)
                                              session-id
                                              #f
                                              "cancellation-token"
                                              iteration))
     (make-loop-result ctx 'cancelled (hasheq 'reason "cancellation-token" 'iteration iteration))]
    [(and shutdown-check (shutdown-check))
     (emit-typed-event! bus
                        (turn-cancelled-event "turn.cancelled"
                                              (current-inexact-milliseconds)
                                              session-id
                                              #f
                                              "graceful-shutdown"
                                              iteration))
     (make-loop-result ctx 'completed (hasheq 'reason "graceful-shutdown" 'iteration iteration))]
    [else #f]))
