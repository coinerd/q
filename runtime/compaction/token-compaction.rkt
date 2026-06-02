#lang racket/base

;; runtime/token-compaction.rkt — Token-based compaction window split (#686-#689)
;; STABILITY: internal
;;
;; Provides token-aware compaction window calculation that replaces
;; fixed-count window splitting with token-budget-based splitting.
;;
;; #686: Backward token walk for compaction boundary
;; #687: Configurable keep-recent-tokens and reserve-tokens
;; #688: Token estimation fallback for messages without usage data
;; #689: Token-based compaction window split (parent)
;;
;; Instead of "keep last N messages", the token-based approach walks
;; messages backward accumulating estimated tokens until the budget is
;; met. Messages beyond the budget are candidates for summarization.

(require racket/contract
         racket/list
         "../../llm/token-budget.rkt"
         "../context/context-policy.rkt")

(provide token-compaction-config
         token-compaction-config?
         token-compaction-config-keep-recent-tokens
         token-compaction-config-reserve-tokens
         token-compaction-config-max-context-tokens
         (contract-out
          [build-token-summary-window
           (-> (listof any/c) token-compaction-config? (values (listof any/c) (listof any/c)))]
          [backward-token-walk
           (-> (listof any/c) exact-nonnegative-integer? (values (listof any/c) (listof any/c)))]
          [estimate-messages-tokens (-> (listof any/c) exact-nonnegative-integer?)]
          [default-token-compaction-config (-> token-compaction-config?)]
          [make-token-compaction-config
           (->* ()
                (#:keep-recent-tokens exact-nonnegative-integer?
                                      #:reserve-tokens exact-nonnegative-integer?
                                      #:max-context-tokens exact-nonnegative-integer?)
                token-compaction-config?)]))

;; ============================================================
;; #687: Token compaction configuration
;; ============================================================

;; Token-based compaction configuration.
;; keep-recent-tokens : how many tokens of recent messages to keep verbatim
;; reserve-tokens     : tokens reserved for system prompt + response headroom
;; max-context-tokens : total context window size (default: 100k)
(struct token-compaction-config (keep-recent-tokens reserve-tokens max-context-tokens) #:transparent)

;; Default: keep 30k tokens of recent messages, reserve 10k for system/response
(define (default-token-compaction-config)
  (token-compaction-config 30000 10000 DEFAULT-TOKEN-BUDGET-THRESHOLD))

;; Constructor with defaults
(define (make-token-compaction-config #:keep-recent-tokens [keep-recent 30000]
                                      #:reserve-tokens [reserve 10000]
                                      #:max-context-tokens [max-ctx DEFAULT-TOKEN-BUDGET-THRESHOLD])
  (token-compaction-config keep-recent reserve max-ctx))

;; ============================================================
;; #688: Token estimation for message lists
;; ============================================================

;; Estimate total tokens for a list of messages.
;; Uses estimate-message-tokens from context-policy which handles
;; text-part extraction and heuristic token estimation.
(define (estimate-messages-tokens messages)
  (for/sum ([m (in-list messages)]) (estimate-message-tokens m)))

;; ============================================================
;; #686: Backward token walk for compaction boundary
;; ============================================================

;; Walk messages from newest to oldest, accumulating tokens.
;; Returns (values kept-messages overflow-messages) where:
;;   kept-messages    : messages that fit within the keep-recent budget (in original order)
;;   overflow-messages : messages beyond the budget (in original order, to be summarized)
;;
;; The walk stops accumulating when adding another message would exceed
;; keep-recent-tokens. Messages beyond that point are overflow candidates.
(define (backward-token-walk messages keep-recent-tokens)
  (define n (length messages))
  ;; Walk from end (index n-1) toward beginning
  (let loop ([i (sub1 n)]
             [used-tokens 0])
    (cond
      ;; All messages fit within budget
      [(< i 0) (values messages '())]
      [else
       (define msg (list-ref messages i))
       (define msg-tokens (estimate-message-tokens msg))
       (define new-total (+ used-tokens msg-tokens))
       (cond
         ;; Still within budget — include this message and continue walking
         [(<= new-total keep-recent-tokens) (loop (sub1 i) new-total)]
         ;; Budget exceeded — messages[0..i] are overflow, messages[i+1..n-1] are kept
         [else
          (define overflow (take messages (add1 i)))
          (define kept (drop messages (add1 i)))
          (values kept overflow)])])))

;; ============================================================
;; #689: Token-based compaction window split
;; ============================================================

;; Split messages into old (to summarize) and recent (to keep) based
;; on token budgets rather than fixed message counts.
;;
;; Strategy:
;;   1. Calculate effective budget: max-context - reserve-tokens
;;   2. Walk backward from newest messages, keeping up to keep-recent-tokens
;;   3. Older messages become the summarization window
;;
;; Returns (values old-messages recent-messages)
(define (build-token-summary-window messages config)
  (define keep-recent (token-compaction-config-keep-recent-tokens config))
  (define reserve (token-compaction-config-reserve-tokens config))
  (define max-ctx (token-compaction-config-max-context-tokens config))

  (define effective-budget (- max-ctx reserve))

  ;; If total messages fit within effective budget, nothing to summarize
  (define total-tokens (estimate-messages-tokens messages))
  (cond
    [(<= total-tokens effective-budget) (values '() messages)]
    [else
     ;; Use backward token walk to find the split point
     (define-values (kept overflow) (backward-token-walk messages keep-recent))
     (values overflow kept)]))
