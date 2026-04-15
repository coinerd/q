#lang racket/base

;; q/runtime/context-builder.rkt — Context assembly from session tree
;;
;; Walks the session tree from active leaf to root, handling
;; compaction summaries, branch summaries, and settings entries.
;; Returns a provider-ready message list for LLM consumption.
;;
;; Issue #498: Context assembly pipeline (tree walk).

(require racket/list
         racket/string
         racket/set
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt"
         "../llm/token-budget.rkt")

(provide build-session-context
         build-session-context/tokens
         truncate-messages-to-budget
         estimate-message-tokens
         entry->context-message)

;; ============================================================
;; Context assembly pipeline (#498)
;; ============================================================

(define (build-session-context idx)
  ;; Build a provider-ready message list from the session index.
  ;; Algorithm:
  ;;   1. Get active leaf from index
  ;;   2. Walk leaf→root collecting path entries
  ;;   3. Find latest compaction-summary on path
  ;;   4. If compaction found → include summary + messages after compaction point
  ;;   5. If no compaction → include all path messages
  ;;   6. Filter out settings/label/bookmark entries
  ;;   7. Transform summaries to user-role messages
  ;;   8. Return provider-ready message list (reversed to root→leaf order)
  ;;
  ;; Returns (listof message?) suitable for LLM provider consumption.
  (define leaf (active-leaf idx))
  (cond
    [(not leaf) '()]
    [else
     (define path (get-branch idx (message-id leaf)))
     (cond
       [(not path) '()]
       ;; path is root→leaf order from get-branch
       [else (assemble-context path)])]))

(define (assemble-context path)
  ;; Process a root→leaf path into context messages.
  ;; Handles compaction summaries and entry type filtering.
  (define-values (pre-compaction post-compaction) (split-at-compaction path))
  (define relevant-entries
    (if post-compaction
        ;; Include compaction summary + everything after it
        post-compaction
        ;; No compaction — use full path
        path))
  ;; Filter and transform entries
  (filter-map entry->context-message relevant-entries))

(define (split-at-compaction path)
  ;; Split path at the last compaction summary.
  ;; Returns (values pre-compaction post-compaction).
  ;; post-compaction includes the compaction summary itself.
  ;; If no compaction found, returns (values path #f).
  ;;
  ;; If the compaction summary has firstKeptEntryId in metadata,
  ;; use it to find the exact start of kept entries.
  (define compaction-idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (cond
    [(not compaction-idx) (values path #f)]
    [else
     (define compaction-entry (list-ref path compaction-idx))
     (define meta (message-meta compaction-entry))
     (define first-kept-id (and (hash? meta) (hash-ref meta 'firstKeptEntryId #f)))
     (cond
       ;; If firstKeptEntryId is set, find it in path and use it as the boundary
       [(and first-kept-id
             (for/or ([entry (in-list path)]
                      [i (in-naturals)])
               (and (equal? (message-id entry) first-kept-id) i)))
        => (lambda (kept-idx)
             (define-values (pre post) (split-at path compaction-idx))
             ;; post = [compaction-summary, ..., first-kept, ...]
             ;; We want [compaction-summary] + messages from first-kept onward
             (values pre post))]
       [else
        (define-values (pre post) (split-at path compaction-idx))
        (values pre post)])]))

(define (entry->context-message entry)
  ;; Convert a session entry to a context message for LLM consumption.
  ;; Returns a message or #f if the entry should be excluded.
  (define kind (message-kind entry))
  (cond
    ;; Standard messages pass through
    [(memq kind '(message)) entry]
    ;; Compaction summaries become user-role messages
    [(eq? kind 'compaction-summary) (transform-summary-to-user entry)]
    ;; Branch summaries become user-role messages
    [(eq? kind 'branch-summary) (transform-summary-to-user entry)]
    ;; Filter out metadata entries
    [(memq kind '(session-info model-change thinking-level-change)) #f]
    ;; Tool results pass through
    [(eq? kind 'tool-result) entry]
    ;; System instructions pass through
    [(eq? kind 'system-instruction) entry]
    ;; Custom messages pass through
    [(eq? kind 'custom-message) entry]
    ;; Unknown kinds pass through (backward compatibility)
    [else entry]))

(define (transform-summary-to-user entry)
  ;; Transform a summary entry into a user-role message for context.
  (struct-copy message entry [role 'user]))

;; ============================================================
;; Token-aware context assembly (#646, #647)
;; ============================================================

;; Estimate token count for a single message struct.
;; Extracts text from all text-parts in the message content.
(define (estimate-message-tokens msg)
  (define text-parts
    (for/list ([part (in-list (message-content msg))]
               #:when (text-part? part))
      (text-part-text part)))
  (estimate-text-tokens (string-join text-parts " ")))

;; Build session context with token budget enforcement.
;; If the assembled messages exceed max-tokens, truncates older messages
;; while preserving system instructions and compaction summaries.
;; Returns (values messages total-tokens)
(define (build-session-context/tokens idx #:max-tokens max-tokens)
  (define messages (build-session-context idx))
  (cond
    [(null? messages) (values '() 0)]
    [else
     (define total (for/sum ([m (in-list messages)]) (estimate-message-tokens m)))
     (cond
       [(<= total max-tokens) (values messages total)]
       [else
        (define truncated (truncate-messages-to-budget messages max-tokens))
        (define new-total (for/sum ([m (in-list truncated)]) (estimate-message-tokens m)))
        (values truncated new-total)])]))

;; Truncate a message list to fit within a token budget.
;; Strategy:
;;   1. Always keep system-instruction and compaction-summary messages
;;   2. Remove oldest non-system messages until within budget
;;   3. Return truncated list
(define (truncate-messages-to-budget messages max-tokens)
  (cond
    [(null? messages) '()]
    [(<= (for/sum ([m (in-list messages)]) (estimate-message-tokens m))
         max-tokens)
     messages]
    [else
     ;; Separate protected (system/compaction) from removable messages
     (define-values (protected removable)
       (partition (lambda (m)
                    (memq (message-kind m) '(system-instruction compaction-summary)))
                  messages))
     ;; Protected messages always included; drop oldest removable until budget met
     (define protected-tokens
       (for/sum ([m (in-list protected)]) (estimate-message-tokens m)))
     (define remaining-budget (- max-tokens protected-tokens))
     (cond
       [(<= remaining-budget 0) protected]
       [else
        ;; Keep most recent removable messages that fit
        (define kept-removable
          (fit-messages-from-recent removable remaining-budget))
        ;; Reassemble in original order: protected + kept-removable
        ;; (preserving order from original messages list)
        (define kept-ids (for/set ([m (in-list kept-removable)]) (message-id m)))
        (for/list ([m (in-list messages)]
                   #:when (or (memq (message-kind m) '(system-instruction compaction-summary))
                              (set-member? kept-ids (message-id m))))
          m)])]))

;; Fit as many messages as possible from the recent end within a token budget.
;; Returns messages in their original order.
(define (fit-messages-from-recent messages budget)
  (let loop ([remaining (reverse messages)]
             [acc '()]
             [used 0])
    (cond
      [(null? remaining) (reverse acc)]
      [else
       (define m (car remaining))
       (define tokens (estimate-message-tokens m))
       (cond
         [(> (+ used tokens) budget) (reverse acc)]
         [else (loop (cdr remaining) (cons m acc) (+ used tokens))])])))

;; ============================================================
;; Utility
;; ============================================================

(define (filter-map f lst)
  (let loop ([lst lst]
             [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [else
       (let ([r (f (car lst))])
         (if r
             (loop (cdr lst) (cons r acc))
             (loop (cdr lst) acc)))])))
