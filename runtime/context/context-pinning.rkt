#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-pinning.rkt — Context message pinning/partitioning
;;
;; Extracted from context-assembly.rkt. Identifies pinned messages
;; (system prompt, first user message, compaction summaries) and
;; partitions them from removable messages.

(require racket/contract
         racket/list
         racket/set
         (only-in "../../util/message.rkt" message-role message-kind message-id))

(provide (contract-out [partition-messages (-> list? (values list? list?))]
                       [partition-messages/working-set (-> list? list? (values list? list?))]))

;; ============================================================
;; Phase 1: Pin system prompt + first user + compaction summaries
;; ============================================================

(define (partition-messages messages)
  (partition-messages/working-set messages '()))

;; Variant that also protects working-set message ids
(define (partition-messages/working-set messages ws-message-ids)
  (define first-user
    (for/first ([m (in-list messages)]
                #:when (eq? (message-role m) 'user))
      m))
  (define ws-id-set (list->set ws-message-ids))
  (define-values (protected removable)
    (partition (lambda (m)
                 (or (eq? (message-kind m) 'system-instruction)
                     (eq? (message-kind m) 'compaction-summary)
                     (eq? (message-kind m) 'context-assembly-summary)
                     (and first-user (eq? m first-user))
                     (set-member? ws-id-set (message-id m))))
               messages))
  (values protected removable))
