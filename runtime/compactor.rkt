#lang racket/base

;; runtime/compactor.rkt — history summarization and compaction
;;
;; Provides:
;;   compaction-strategy          — struct: summary-window-size, keep-recent-count
;;   compaction-result            — struct: summary-message, removed-count, kept-messages
;;   compact-history              — summarize history and produce compaction entries (ADVISORY-ONLY)
;;   compact-history-advisory     — explicit alias for compact-history (advisory, no filesystem)
;;   compact-and-persist!         — compact history AND write summary entry to session log
;;   build-summary-window         — determine what to summarize vs what to keep recent
;;   write-compaction-entry!      — write a compaction entry to session log
;;   compaction-result->message-list — flatten result into a single message list
;;   default-strategy             — default compaction strategy
;;   default-summarize            — simple concatenation summarizer
;;
;; Key invariant: compaction NEVER silently destroys prior history.
;; The original log remains reconstructible. compact-history produces
;; a NEW message list without modifying or deleting the originals.

(require racket/contract
         racket/string
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         (only-in "../util/hook-types.rkt" hook-result hook-result? hook-result-action hook-result-payload))

(provide
 (struct-out compaction-strategy)
 (struct-out compaction-result)
 ;; Compaction functions (WP-37: now also exports compact-history for tests)
 compact-history
 compact-history-advisory
 compact-and-persist!
 build-summary-window
 write-compaction-entry!
 compaction-result->message-list
 default-strategy
 default-summarize
 ;; Tiered Context Assembly (WP-37)
 (struct-out tiered-context)
 build-tiered-context
 tiered-context->message-list
 ;; R2-6: Context Assembly Hooks
 build-tiered-context-with-hooks
 (struct-out context-assembly-payload)
 payload->tiered-context
 tiered-context->payload)

;; ============================================================
;; Structs
;; ============================================================

;; Compaction strategy parameters
(struct compaction-strategy (summary-window-size keep-recent-count)
  #:transparent)
;; summary-window-size : integer — max number of past messages to summarize
;; keep-recent-count   : integer — how many recent messages to keep as-is

;; Compaction result
(struct compaction-result (summary-message removed-count kept-messages)
  #:transparent)
;; summary-message : (or/c message? #f) — a single 'system message with the summary text, or #f if nothing was summarized
;; removed-count   : integer — how many messages were replaced by the summary
;; kept-messages   : (listof message?) — the recent messages that were kept verbatim

;; ============================================================
;; Default strategy and summarize function
;; ============================================================

(define (default-strategy)
  (compaction-strategy 50 20))

(define (default-summarize messages)
  ;; Simple summarization: produce a structured summary string.
  ;; Concatenates the text content of each message.
  ;; In production, this would call the LLM to produce a real summary.
  (define n (length messages))
  (define parts
    (for/list ([m (in-list messages)])
      (define content (message-content m))
      (define text
        (string-join
          (for/list ([part (in-list content)]
                     #:when (text-part? part))
            (text-part-text part))
          " "))
      (format "[~a] ~a" (message-role m) text)))
  (format "[Compaction summary of ~a messages]\n~a"
          n
          (string-join parts "\n")))

;; ============================================================
;; build-summary-window
;; ============================================================

;; Split messages into old (to summarize) and recent (to keep).
;; The "old" portion is further capped at summary-window-size.
;; Returns: (values old-messages recent-messages)
(define (build-summary-window messages strategy)
  (define total (length messages))
  (define keep-recent (compaction-strategy-keep-recent-count strategy))
  (define window-size (compaction-strategy-summary-window-size strategy))
  (cond
    [(<= total keep-recent)
     ;; All messages fit in the "recent" window — nothing to summarize
     (values '() messages)]
    [else
     (define split-point (max 0 (- total keep-recent)))
     (define all-old (take messages split-point))
     (define recent (drop messages split-point))
     ;; Cap the old messages at summary-window-size
     ;; (take the LAST window-size of the old messages, to summarize the most recent of the old)
     (define old-to-summarize
       (if (> (length all-old) window-size)
           (take-right all-old window-size)
           all-old))
     (values old-to-summarize recent)]))

;; ============================================================
;; compact-history
;; ============================================================

;; Main compaction function.
;; Returns a compaction-result. Does NOT modify the original log.
;;
;; CONTRACT: This function is ADVISORY-ONLY. It does NOT modify the session log
;; or touch the filesystem in any way. To persist the compaction, use
;; `compact-and-persist!` or call `write-compaction-entry!` explicitly.
(define (compact-history messages
                         #:strategy [strategy (default-strategy)]
                         #:summarize-fn [summarize-fn default-summarize])
  (define-values (old recent) (build-summary-window messages strategy))
  (cond
    [(null? old)
     ;; Nothing to summarize — return identity result
     (compaction-result #f 0 recent)]
    [else
     ;; Summarize the old messages into a single system message
     (define summary-text (summarize-fn old))
     (define summary-msg
       (make-message
        (format "compaction-~a" (current-inexact-milliseconds))
        #f  ; no parent — compaction summaries are root-level context
        'system
        'compaction-summary
        (list (make-text-part summary-text))
        (current-seconds)
        (hasheq 'type "compaction"
                'removedCount (length old))))
     (compaction-result summary-msg (length old) recent)]))

;; ============================================================
;; compaction-result->message-list
;; ============================================================

;; Flatten a compaction-result into a single message list.
;; If a summary exists, prepends it before the kept messages.
(define (compaction-result->message-list result)
  (define summary (compaction-result-summary-message result))
  (define kept (compaction-result-kept-messages result))
  (if summary
      (cons summary kept)
      kept))

;; ============================================================
;; compact-history-advisory
;; ============================================================

;; Explicit advisory variant — returns the result but does NOT write
;; to the session log. This is a pure function that does not touch
;; the filesystem. Equivalent to `compact-history` but named to make
;; the advisory contract crystal clear.
(define (compact-history-advisory messages
                                   #:strategy [strategy (default-strategy)]
                                   #:summarize-fn [summarize-fn default-summarize])
  (compact-history messages #:strategy strategy #:summarize-fn summarize-fn))

;; ============================================================
;; compact-and-persist!
;; ============================================================

;; Compacts history AND persists the summary entry to the session log.
;; Returns the compaction-result for inspection.
;; This is the "persisting" variant — the summary entry becomes part of
;; the durable session history.
;; The original messages are NOT deleted — they remain in the log.
;; Future context assembly can use the summary + recent messages
;; instead of replaying the full history.
(define (compact-and-persist! messages session-log-path
                               #:strategy [strategy (default-strategy)]
                               #:summarize-fn [summarize-fn default-summarize])
  (define result (compact-history messages #:strategy strategy #:summarize-fn summarize-fn))
  (write-compaction-entry! session-log-path result)
  result)

;; ============================================================
;; write-compaction-entry!
;; ============================================================

;; Write a compaction-summary entry to the session log.
;; The entry records what was summarized. Original entries remain intact.
;; If no compaction happened (summary is #f), this is a no-op.
(define (write-compaction-entry! session-path compaction-res)
  (define summary (compaction-result-summary-message compaction-res))
  (when summary
    (append-entry! session-path summary)))

;; ============================================================
;; Tiered Context Assembly (WP-37)
;; ============================================================

;; Tiered context holds messages split into three tiers:
;; - Tier A: Compacted summaries of older messages
;; - Tier B: Recent messages kept verbatim (not compacted)
;; - Tier C: Current turn messages (most recent, always included in full)
(struct tiered-context (tier-a tier-b tier-c)
  #:transparent)

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; R2-6: Context Assembly Hook Payload
;; Serializable payload for context-assembly hook point
(struct context-assembly-payload
  (tier-a-messages tier-b-messages tier-c-messages max-tokens metadata)
  #:transparent)

;; Helper: Convert payload back to tiered-context
(define (payload->tiered-context payload)
  (tiered-context
   (context-assembly-payload-tier-a-messages payload)
   (context-assembly-payload-tier-b-messages payload)
   (context-assembly-payload-tier-c-messages payload)))

;; Helper: Convert tiered-context to payload
(define (tiered-context->payload tc max-tokens [metadata (hasheq)])
  (context-assembly-payload
   (tiered-context-tier-a tc)
   (tiered-context-tier-b tc)
   (tiered-context-tier-c tc)
   max-tokens
   metadata))

;; Split messages into three tiers based on compaction status and recency.
;; Parameters:
;;   messages - list of messages (may include compaction-summary messages)
;;   tier-b-count - how many recent non-summary messages to keep in Tier B
;;   tier-c-count - how many most recent messages to treat as "current turn" (Tier C)
;; Returns: tiered-context struct
(define (build-tiered-context messages
                               #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                               #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT])
  ;; Separate compaction summaries (Tier A candidates) from regular messages
  (define-values (compaction-summaries regular-msgs)
    (partition (lambda (m) (eq? (message-kind m) 'compaction-summary))
               messages))

  ;; Calculate how many messages go to Tier C (most recent)
  (define total (length regular-msgs))
  (define tier-c-size (min tier-c-count total))

  ;; Tier C: most recent messages
  (define tier-c
    (if (> tier-c-size 0)
        (take-right regular-msgs tier-c-size)
        '()))

  ;; Remaining messages after Tier C
  (define remaining-after-c
    (if (> tier-c-size 0)
        (drop-right regular-msgs tier-c-size)
        regular-msgs))

  ;; Tier B: recent messages (up to tier-b-count)
  (define remaining-count (length remaining-after-c))
  (define tier-b-size (min tier-b-count remaining-count))

  (define tier-b
    (if (> tier-b-size 0)
        (take-right remaining-after-c tier-b-size)
        '()))

  ;; Any remaining regular messages would conceptually go to Tier A
  ;; but we only include explicit compaction summaries in Tier A
  ;; (those have already been processed into summaries)

  (tiered-context compaction-summaries tier-b tier-c))

;; R2-6: Build tiered context with hook support
;; Parameters:
;;   messages - list of messages
;;   hook-dispatcher - optional function (hook-point payload) -> hook-result
;;   tier-b-count, tier-c-count - tier boundaries
;;   max-tokens - max tokens for context (for payload)
;; Returns: (values tiered-context hook-result-or-#f)
;; Raises: exn:fail if hook returns 'block
(define (build-tiered-context-with-hooks
         messages
         #:hook-dispatcher [hook-dispatcher #f]
         #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
         #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
         #:max-tokens [max-tokens 8192])
  ;; First build the base tiered context
  (define base-context (build-tiered-context messages
                                              #:tier-b-count tier-b-count
                                              #:tier-c-count tier-c-count))

  ;; Create the hook payload (serializable for logging)
  (define payload (tiered-context->payload base-context max-tokens
                                           (hasheq 'tier-b-count tier-b-count
                                                   'tier-c-count tier-c-count
                                                   'total-messages (length messages))))

  ;; Dispatch hook if dispatcher provided
  (if hook-dispatcher
      (let ([result (hook-dispatcher 'context-assembly payload)])
        (case (hook-result-action result)
          [(block)
           ;; Hook blocked - raise error with reason
           (define reason (hook-result-payload result))
           (raise (exn:fail (format "Context assembly blocked by hook: ~a"
                                    (if reason reason "no reason given"))
                            (current-continuation-marks)))]
          [(amend)
           ;; Hook amended - use new payload to build tiered context
           (define amended-payload (hook-result-payload result))
           (values (payload->tiered-context amended-payload) result)]
          [(pass)
           ;; Hook passed - use original context
           (values base-context result)]
          [else
           ;; Unknown action - use original context
           (values base-context result)]))
      ;; No hook dispatcher - return base context
      (values base-context #f)))

;; Flatten tiered context back into a single message list.
;; Order: Tier A (summaries) → Tier B (recent) → Tier C (current)
(define (tiered-context->message-list tc)
  (append (tiered-context-tier-a tc)
          (tiered-context-tier-b tc)
          (tiered-context-tier-c tc)))
