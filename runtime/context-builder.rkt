#lang racket/base

;; q/runtime/context-builder.rkt — Context assembly from session tree
;;
;; Walks the session tree from active leaf to root, handling
;; compaction summaries, branch summaries, and settings entries.
;; Returns a provider-ready message list for LLM consumption.
;;
;; Issue #498: Context assembly pipeline (tree walk).

(require racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt")

(provide build-session-context
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
  (define compaction-idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (cond
    [(not compaction-idx) (values path #f)]
    [else
     (define-values (pre post) (split-at path compaction-idx))
     (values pre post)]))

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
