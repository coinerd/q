#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-pinning.rkt — Context message pinning/partitioning
;;
;; Extracted from context-assembly.rkt. Identifies pinned messages
;; (system prompt, first user message, compaction summaries) and
;; partitions them from removable messages.

(require racket/list
         (only-in "../util/protocol-types.rkt" message-role message-kind))

(provide partition-messages)

;; ============================================================
;; Phase 1: Pin system prompt + first user + compaction summaries
;; ============================================================

(define (partition-messages messages)
  (define first-user
    (for/first ([m (in-list messages)]
                #:when (eq? (message-role m) 'user))
      m))
  (define-values (protected removable)
    (partition (lambda (m)
                 (or (eq? (message-kind m) 'system-instruction)
                     (eq? (message-kind m) 'compaction-summary)
                     (eq? (message-kind m) 'context-assembly-summary)
                     (and first-user (eq? m first-user))))
               messages))
  (values protected removable))
