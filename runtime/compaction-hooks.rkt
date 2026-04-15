#lang racket/base

;; runtime/compaction-hooks.rkt — Enriched Compaction Hook and Events (#697-#699)
;;
;; #697: Enrich session-before-compact hook with preparation data.
;;       Payload includes: messages-to-summarize, turn-prefix,
;;       previous-summary, file-ops, tokens-before.
;;       Extensions can return a custom summary via hook-result-payload.
;;
;; #698: Add compaction-start and compaction-end events.
;;       TUI subscribes for progress indicator.
;;       Payload: reason, message-count, tokens-before/after.
;;
;; #699: Parent feature combining both.

(require racket/contract
         racket/list
         racket/match
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         "compactor.rkt"
         "split-turn.rkt"
         "token-compaction.rkt")

(provide
 ;; #697: Enriched hook
 build-enriched-compact-payload
 dispatch-enriched-before-compact
 maybe-use-custom-summary
 ;; #698: Compaction events
 publish-compaction-start!
 publish-compaction-end!
 compaction-start-topic
 compaction-end-topic
 ;; Event construction helpers
 make-compaction-start-event
 make-compaction-end-event)

;; ============================================================
;; Constants
;; ============================================================

(define compaction-start-topic "compaction.start")
(define compaction-end-topic "compaction.end")

;; ============================================================
;; #697: Enriched hook payload
;; ============================================================

;; Build the enriched payload for session-before-compact.
;; This gives extensions full context about what's about to be compacted.
(define (build-enriched-compact-payload messages strategy
                                        #:previous-summary [prev-summary #f]
                                        #:session-id [session-id "unknown"])
  (define total (length messages))
  (define-values (old recent) (build-summary-window messages strategy))
  (define split-idx (length old))
  ;; Check for split-turn
  (define split-info (find-split-turn messages split-idx))
  (define turn-prefix-text
    (if (split-turn-result-is-split? split-info)
        (generate-turn-prefix (split-turn-result-turn-messages split-info))
        ""))
  ;; Estimate tokens
  (define tokens-before (estimate-messages-tokens messages))
  ;; Build payload hash
  (hasheq 'message-count total
          'messages-to-summarize (length old)
          'messages-to-keep (length recent)
          'turn-prefix turn-prefix-text
          'previous-summary (or prev-summary "")
          'tokens-before tokens-before
          'split-index split-idx
          'is-split-turn (split-turn-result-is-split? split-info)
          'session-id session-id
          'strategy strategy))

;; Dispatch the enriched before-compact hook.
;; Returns (values hook-result-or-#f enriched-payload)
;; The hook-result may contain a custom summary in its payload.
(define (dispatch-enriched-before-compact hook-dispatcher messages strategy
                                          #:previous-summary [prev-summary #f]
                                          #:session-id [session-id "unknown"])
  (define payload (build-enriched-compact-payload messages strategy
                                                   #:previous-summary prev-summary
                                                   #:session-id session-id))
  (define hook-res
    (and hook-dispatcher
         (hook-dispatcher 'session-before-compact payload)))
  (values hook-res payload))

;; If the hook returned a custom summary, extract it.
;; Returns #f if no custom summary, or the summary string.
(define (maybe-use-custom-summary hook-res)
  (and (hook-result? hook-res)
       (eq? (hook-result-action hook-res) 'replace)
       (hash-ref (hook-result-payload hook-res) 'summary #f)))

;; ============================================================
;; #698: Compaction events
;; ============================================================

;; Create a compaction-start event.
(define (make-compaction-start-event reason message-count tokens-before
                                     session-id turn-id)
  (make-event compaction-start-topic
              (current-inexact-milliseconds)
              session-id
              turn-id
              (hasheq 'reason reason
                      'message-count message-count
                      'tokens-before tokens-before)))

;; Create a compaction-end event.
(define (make-compaction-end-event reason removed-count tokens-before tokens-after
                                   session-id turn-id
                                   #:summary-generated? [summary-gen? #t])
  (make-event compaction-end-topic
              (current-inexact-milliseconds)
              session-id
              turn-id
              (hasheq 'reason reason
                      'removed-count removed-count
                      'tokens-before tokens-before
                      'tokens-after tokens-after
                      'summary-generated? summary-gen?)))

;; Publish a compaction-start event to the event bus.
(define (publish-compaction-start! bus reason message-count tokens-before
                                   session-id turn-id)
  (when bus
    (define evt (make-compaction-start-event reason message-count tokens-before
                                             session-id turn-id))
    (publish! bus evt)))

;; Publish a compaction-end event to the event bus.
(define (publish-compaction-end! bus reason removed-count tokens-before tokens-after
                                 session-id turn-id
                                 #:summary-generated? [summary-gen? #t])
  (when bus
    (define evt (make-compaction-end-event reason removed-count tokens-before tokens-after
                                           session-id turn-id
                                           #:summary-generated? summary-gen?))
    (publish! bus evt)))
