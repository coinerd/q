#lang racket/base

;; runtime/context-policy.rkt — Shared context policy functions
;; STABILITY: evolving
;;
;; Extracted from context-builder.rkt and context-manager.rkt (v0.23.0 W0).
;; Single implementations of token estimation, pinning, and pair-preserving
;; budget fitting used by both context assembly paths.
;;
;; Issue #2402: W0 — Extract context-policy.rkt

(require racket/contract
         racket/list
         racket/string
         racket/set
         "../util/protocol-types.rkt"
         "../llm/token-budget.rkt")

;; Token estimation
(provide (contract-out [estimate-message-tokens (-> message? exact-nonnegative-integer?)]
                       [ensure-first-user-pinned
                        (-> (listof message?) (listof message?) (listof message?))]
                       [fit-messages-pair-preserving
                        (->* [(listof message?) exact-nonnegative-integer?]
                             [(or/c #f procedure?)]
                             (listof message?))]
                       [system-message? (-> message? boolean?)]
                       [user-message? (-> message? boolean?)])
         ;; Internal helpers (not contracted — used by context-assembly only)
         build-pair-index
         requires-pair-inclusion?
         ;; Re-export from token-budget
         estimate-text-tokens)

;; ============================================================
;; Token estimation
;; ============================================================

;; Estimate token count for a single message struct.
;; Extracts text from all text-parts in the message content.
;; This is the canonical version (from context-builder, more complete).
(define (estimate-message-tokens msg)
  (define text-parts
    (for/list ([part (in-list (message-content msg))]
               #:when (text-part? part))
      (text-part-text part)))
  (estimate-text-tokens (string-join text-parts " ")))

;; ============================================================
;; Predicates
;; ============================================================

(define (system-message? msg)
  (eq? (message-kind msg) 'system-instruction))

(define (user-message? msg)
  (eq? (message-role msg) 'user))

;; ============================================================
;; Pinning
;; ============================================================

;; Ensure the first user message is present in the result list.
;; Merged best of both implementations: searches result for first-user,
;; if missing, inserts at its original position from the source list.
(define (ensure-first-user-pinned result original)
  (define first-user
    (for/first ([m (in-list original)]
                #:when (user-message? m))
      m))
  (cond
    [(not first-user) result]
    [(member first-user result) result]
    [else
     (define target-id (message-id first-user))
     (define after-id
       (for/first ([m (in-list (dropf original (lambda (m) (not (equal? (message-id m) target-id)))))]
                   #:when (member m result))
         (message-id m)))
     (cond
       [after-id
        (let loop ([acc '()]
                   [rem result])
          (cond
            [(null? rem) (reverse (cons first-user acc))]
            [(equal? (message-id (car rem)) after-id)
             (loop (cons (car rem) (cons first-user acc)) (cdr rem))]
            [else (loop (cons (car rem) acc) (cdr rem))]))]
       [else (cons first-user result)])]))

;; ============================================================
;; Pair-preserving budget fitting
;; ============================================================

;; Build index of tool-call/tool-result pairs in a message list.
;; Returns: (values tr->assistant assistant->trs)
;;   tr->assistant: hash mapping tool-result-id → assistant-msg-id
;;   assistant->trs: hash mapping assistant-msg-id → list of tool-result-msg-ids
(define (build-pair-index messages)
  (define msg-ids
    (for/set ([m (in-list messages)])
      (message-id m)))
  (define tr->assistant (make-hash))
  (define assistant->trs (make-hash))
  (for ([m (in-list messages)])
    (cond
      ;; Tool result → parent assistant
      [(eq? (message-role m) 'tool)
       (define pid (message-parent-id m))
       (when (and pid (set-member? msg-ids pid))
         (hash-set! tr->assistant (message-id m) pid)
         (hash-update! assistant->trs pid (lambda (lst) (cons (message-id m) lst)) '()))]
      ;; Assistant with tool_call parts
      [(eq? (message-role m) 'assistant)
       (define parts (message-content m))
       (when (and (pair? parts) (ormap tool-call-part? parts))
         (when (not (hash-has-key? assistant->trs (message-id m)))
           (hash-set! assistant->trs (message-id m) '())))]))
  (values tr->assistant assistant->trs))

;; Check if a message has pair dependencies that require inclusion.
(define (requires-pair-inclusion? msg-id tr->assistant assistant->trs)
  (or (hash-has-key? tr->assistant msg-id) (hash-has-key? assistant->trs msg-id)))

;; Fit as many messages as possible from the recent end within a token budget,
;; preserving tool-call/tool-result pairing.
;; Returns two values: (values kept-messages excluded-messages).
;; Optional estimate-fn overrides the token estimation function.
(define (fit-messages-pair-preserving messages budget [estimate-fn estimate-message-tokens])
  (define-values (tr->assistant assistant->trs) (build-pair-index messages))
  (let loop ([remaining (reverse messages)]
             [acc '()]
             [acc-ids (set)]
             [used 0])
    (cond
      [(null? remaining) (reverse acc)]
      [else
       (define m (car remaining))
       (define mid (message-id m))
       (cond
         ;; Already included (as pair dependency)
         [(set-member? acc-ids mid) (loop (cdr remaining) acc acc-ids used)]
         [else
          (define tokens (estimate-fn m))
          ;; Calculate pair tokens
          (define pair-tokens 0)
          ;; Tool result → include parent assistant
          (define required-assistant (hash-ref tr->assistant mid #f))
          (when required-assistant
            (unless (set-member? acc-ids required-assistant)
              (define ass-msg
                (for/first ([mm (in-list messages)]
                            #:when (equal? (message-id mm) required-assistant))
                  mm))
              (when ass-msg
                (set! pair-tokens (+ pair-tokens (estimate-fn ass-msg))))))
          ;; Assistant with tool_calls → include child tool results
          (define child-trs (hash-ref assistant->trs mid #f))
          (when child-trs
            (for ([tr-id (in-list child-trs)])
              (unless (set-member? acc-ids tr-id)
                (define tr-msg
                  (for/first ([mm (in-list messages)]
                              #:when (equal? (message-id mm) tr-id))
                    mm))
                (when tr-msg
                  (set! pair-tokens (+ pair-tokens (estimate-fn tr-msg)))))))
          (define total-needed (+ used tokens pair-tokens))
          (cond
            [(> total-needed budget) (reverse acc)]
            [else
             ;; Include this message and its required pairs
             (define new-acc (cons m acc))
             (define new-ids (set-add acc-ids mid))
             (define new-used (+ used tokens))
             ;; Include parent assistant if needed
             (define-values (final-acc final-ids final-used)
               (if (and required-assistant (not (set-member? new-ids required-assistant)))
                   (let ([ass-msg (for/first ([mm (in-list messages)]
                                              #:when (equal? (message-id mm) required-assistant))
                                    mm)])
                     (if ass-msg
                         (values (cons ass-msg new-acc)
                                 (set-add new-ids required-assistant)
                                 (+ new-used (estimate-fn ass-msg)))
                         (values new-acc new-ids new-used)))
                   (values new-acc new-ids new-used)))
             ;; Include child tool results if needed
             (define-values (final-acc2 final-ids2 final-used2)
               (if child-trs
                   (for/fold ([fa final-acc]
                              [fi final-ids]
                              [fu final-used])
                             ([tr-id (in-list child-trs)])
                     (if (set-member? fi tr-id)
                         (values fa fi fu)
                         (let ([tr-msg (for/first ([mm (in-list messages)]
                                                   #:when (equal? (message-id mm) tr-id))
                                         mm)])
                           (if tr-msg
                               (values (cons tr-msg fa)
                                       (set-add fi tr-id)
                                       (+ fu (estimate-fn tr-msg)))
                               (values fa fi fu)))))
                   (values final-acc final-ids final-used)))
             (loop (cdr remaining) final-acc2 final-ids2 final-used2)])])])))
