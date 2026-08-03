#lang racket/base

;; runtime/adaptive-retry.rkt — pure PN-6 request-reduction policy
;; STABILITY: internal

(require racket/contract/base
         (only-in "../util/message/message.rkt" message-role))

(provide (contract-out [adaptive-network-error-type? (-> symbol? boolean?)]
                       [adapt-provider-request (-> list? hash? (values list? hash? boolean?))]))

(define (adaptive-network-error-type? error-type)
  (and (memq error-type '(timeout network)) #t))

(define (user-assistant-pair-at-front? messages)
  (and (pair? messages)
       (pair? (cdr messages))
       (eq? (message-role (car messages)) 'user)
       (eq? (message-role (cadr messages)) 'assistant)))

(define (contains-user-assistant-pair? messages)
  (cond
    [(or (null? messages) (null? (cdr messages))) #f]
    [(user-assistant-pair-at-front? messages) #t]
    [else (contains-user-assistant-pair? (cdr messages))]))

;; Drop the oldest user/assistant pair only when another complete pair remains.
;; System messages and the current trailing user request are never removed.
(define (drop-oldest-removable-pair messages)
  (let loop ([prefix '()]
             [remaining messages])
    (cond
      [(or (null? remaining) (null? (cdr remaining))) #f]
      [(and (user-assistant-pair-at-front? remaining)
            (contains-user-assistant-pair? (cddr remaining)))
       (append (reverse prefix) (cddr remaining))]
      [else (loop (cons (car remaining) prefix) (cdr remaining))])))

(define (reduce-max-tokens settings)
  (define max-tokens (hash-ref settings 'max-tokens #f))
  (if (exact-positive-integer? max-tokens)
      (hash-set settings 'max-tokens (max 1 (floor (* max-tokens 3/4))))
      settings))

;; Returns reduced context/settings and whether adaptation occurred.
;; At the minimum context floor, both inputs are returned unchanged.
(define (adapt-provider-request ctx settings)
  (define trimmed (drop-oldest-removable-pair ctx))
  (if trimmed
      (values trimmed (reduce-max-tokens settings) #t)
      (values ctx settings #f)))
