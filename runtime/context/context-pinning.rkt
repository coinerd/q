#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-pinning.rkt — Context message pinning/partitioning
;;
;; Extracted from context-assembly.rkt. Identifies pinned messages
;; (system prompt, ALL user messages, compaction summaries) and
;; partitions them from removable messages.
;;
;; UNIVERSAL PINNING FIX: ALL user messages are now pinned (not just
;; the first). User messages are tiny (~40 tokens), rare, and define
;; task intent. Dropping any user message can cause task regression
;; in multi-prompt sessions.

(require racket/contract
         racket/list
         racket/set
         (only-in "../../util/message/message.rkt" message-role message-kind message-id))

(provide (contract-out [partition-messages (-> list? (values list? list?))]
                       [partition-messages/working-set (-> list? list? (values list? list?))]))

;; ============================================================
;; Phase 1: Pin system prompt + ALL user messages + compaction summaries
;; ============================================================

(define (partition-messages messages)
  (partition-messages/working-set messages '()))

;; Variant that also protects working-set message ids.
;; UNIVERSAL PINNING: Protect ALL user messages, not just the first.
(define (partition-messages/working-set messages ws-message-ids)
  (define ws-id-set (list->set ws-message-ids))
  (define-values (protected removable)
    (partition (lambda (m)
                 (or (eq? (message-kind m) 'system-instruction)
                     (eq? (message-kind m) 'compaction-summary)
                     (eq? (message-kind m) 'context-assembly-summary)
                     (eq? (message-role m) 'user) ; UNIVERSAL: pin ALL user messages
                     (set-member? ws-id-set (message-id m))))
               messages))
  (values protected removable))
