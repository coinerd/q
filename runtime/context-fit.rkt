#lang racket/base

;; runtime/context-fit.rkt — Token budget fitting for context assembly (RA-3b, v0.24.8)
;;
;; Extracted from context-assembly.rkt to reduce surface area.
;; Functions for fitting messages within token budgets while preserving
;; system instructions and first-user pinning.

(require racket/list
         racket/set
         (only-in "../util/protocol-types.rkt" message-role message-kind message-id)
         (only-in "../runtime/context-policy.rkt"
                  estimate-message-tokens
                  ensure-first-user-pinned
                  fit-messages-pair-preserving))

(provide truncate-messages-to-budget
         fit-messages-from-recent)

;; Fit messages from recent end within budget (pair-preserving).
(define (fit-messages-from-recent messages budget)
  (fit-messages-pair-preserving messages budget))

;; truncate-messages-to-budget: Trim messages from the front to fit within token budget.
;; Returns truncated message list with first-user pinning preserved.
(define (truncate-messages-to-budget messages max-tokens)
  (cond
    [(null? messages) '()]
    [(<= (for/sum ([m (in-list messages)]) (estimate-message-tokens m)) max-tokens) messages]
    [else
     (define-values (protected removable)
       (partition (lambda (m) (memq (message-kind m) '(system-instruction compaction-summary)))
                  messages))
     (define first-user-msg
       (for/first ([m (in-list messages)]
                   #:when (eq? (message-role m) 'user))
         m))
     (define protected-tokens (for/sum ([m (in-list protected)]) (estimate-message-tokens m)))
     (define pinned-tokens
       (if (and first-user-msg (not (member first-user-msg protected)))
           (estimate-message-tokens first-user-msg)
           0))
     (define remaining-budget (- max-tokens protected-tokens pinned-tokens))
     (cond
       [(<= remaining-budget 0) (ensure-first-user-pinned protected messages)]
       [else
        (define kept-removable (fit-messages-from-recent removable remaining-budget))
        (define kept-ids
          (for/set ([m (in-list kept-removable)])
            (message-id m)))
        (ensure-first-user-pinned
         (for/list ([m (in-list messages)]
                    #:when (or (memq (message-kind m) '(system-instruction compaction-summary))
                               (set-member? kept-ids (message-id m))))
           m)
         messages)])]))
