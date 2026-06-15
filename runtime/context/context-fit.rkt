#lang racket/base

;; runtime/context-fit.rkt — Token budget fitting for context assembly (RA-3b, v0.24.8)
;;
;; Extracted from context-assembly.rkt to reduce surface area.
;; Functions for fitting messages within token budgets while preserving
;; system instructions and ALL user message pinning.

(require racket/contract
         racket/list
         racket/set
         (only-in "../../util/message/message.rkt" message-role message-kind message-id)
         (only-in "context-policy.rkt"
                  estimate-message-tokens
                  estimate-message-tokens-cached
                  ensure-user-messages-pinned
                  fit-messages-with-importance-rescue))

(provide (contract-out
          [truncate-messages-to-budget (-> (listof any/c) exact-nonnegative-integer? (listof any/c))]
          [fit-messages-from-recent (-> (listof any/c) exact-nonnegative-integer? (listof any/c))]))

;; Fit messages from recent end within budget (pair-preserving + importance rescue).
;; v0.45.7 (NF2): Now uses importance-aware rescue to preserve critical/high messages.
(define (fit-messages-from-recent messages budget)
  (fit-messages-with-importance-rescue messages budget))

;; truncate-messages-to-budget: Trim messages from the front to fit within token budget.
;; Returns truncated message list with ALL user message pinning preserved.
(define (truncate-messages-to-budget messages max-tokens)
  (cond
    [(null? messages) '()]
    [(<= (for/sum ([m (in-list messages)]) (estimate-message-tokens-cached m)) max-tokens) messages]
    [else
     ;; Partition into protected (system, summaries, ALL user messages) and removable
     (define-values (protected removable)
       (partition (lambda (m)
                    (or (memq (message-kind m) '(system-instruction compaction-summary))
                        (eq? (message-role m) 'user)))
                  messages))
     (define protected-tokens (for/sum ([m (in-list protected)]) (estimate-message-tokens-cached m)))
     (define remaining-budget (- max-tokens protected-tokens))
     (cond
       [(<= remaining-budget 0) (ensure-user-messages-pinned protected messages)]
       [else
        (define kept-removable (fit-messages-from-recent removable remaining-budget))
        (define kept-ids
          (for/set ([m (in-list kept-removable)])
            (message-id m)))
        (ensure-user-messages-pinned
         (for/list ([m (in-list messages)]
                    #:when (or (memq (message-kind m) '(system-instruction compaction-summary))
                               (eq? (message-role m) 'user)
                               (set-member? kept-ids (message-id m))))
           m)
         messages)])]))
