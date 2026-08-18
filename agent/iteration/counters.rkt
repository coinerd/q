#lang racket/base

;; agent/iteration/counters.rkt — iteration counter logic + cancellation check
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
                  loop-counters-edited-paths
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count
                  loop-counters-implement-count
                  loop-counters-consecutive-error-count
                  loop-counters-recent-tool-names)
         (only-in "../../util/content/content-parts.rkt" tool-result-part?)
         (only-in "../../util/message/message.rkt" message-content)
         (only-in "../../util/tool/tool-types.rkt" tool-call-name)
         (only-in "../../util/content/content-parts.rkt" tool-result-part-is-error?)
         (only-in "../../util/tool/tool-extract.rkt" extract-tool-calls-from-messages)
         (only-in "tool-turn-bridge.rkt" update-seen-paths take-at-most extract-tool-target-path)
         (only-in "../event-emitter.rkt" emit-typed-event!)
         (only-in "../event-structs/hook-events.rkt" turn-cancelled-event)
         (only-in "../../util/loop-result.rkt" make-loop-result)
         (only-in "../../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?))

(provide (contract-out [compute-next-counters (-> any/c (listof any/c) any/c)]
                       [check-cancellation (-> any/c any/c any/c any/c any/c any/c any/c any/c)]))

;; ============================================================
;; compute-next-counters
;; ============================================================

(define (compute-next-counters counters new-msgs)
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  (define-values (new-seen-paths _new-exploration-path?)
    (update-seen-paths current-tool-calls (loop-counters-seen-paths counters)))
  ;; BUG-0016: progress-aware consecutive-tool breaker. A turn that edits a
  ;; NEW distinct file (a path not already edited in the current streak) is
  ;; implementation progress, not circling — reset the streak so a bulk
  ;; migration touching hundreds of distinct files is never policy-killed.
  (define edited-now
    (for*/list ([tc (in-list current-tool-calls)]
                #:when (member (tool-call-name tc) '("edit" "write"))
                [p (in-value (extract-tool-target-path tc))]
                #:when (and p (string? p) (not (string=? p ""))))
      p))
  (define distinct-file-edit?
    (for/or ([p (in-list edited-now)])
      (not (member p (loop-counters-edited-paths counters)))))
  (define new-edited-paths
    (remove-duplicates (append edited-now (loop-counters-edited-paths counters))))
  ;; This counter is the number of consecutive assistant turns that emitted
  ;; one or more tool calls. It is independent of path novelty and tool class,
  ;; and resets as soon as a turn emits no tool calls OR edits a distinct file.
  (define effective-tool-count
    (cond
      ;; No messages means no assistant turn occurred; preserve the counter.
      [(null? new-msgs) (loop-counters-consecutive-tool-count counters)]
      ;; A distinct-file edit is progress — reset the streak.
      [distinct-file-edit? 0]
      [(pair? current-tool-calls) (add1 (loop-counters-consecutive-tool-count counters))]
      [else 0]))
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
               [edited-paths
                (if (null? new-msgs)
                    (loop-counters-edited-paths counters)
                    new-edited-paths)]
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
