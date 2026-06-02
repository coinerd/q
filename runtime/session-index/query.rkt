#lang racket/base

;; runtime/session-index/query.rkt — lookup, filter, navigation operations
;;
;; Read-only query operations on session-index.

(require racket/contract
         racket/list
         racket/set
         (only-in "../../util/content-parts.rkt" text-part?)
         (only-in "../../util/message.rkt" message? message-id message-parent-id message-content)
         (only-in "../../util/content-parts.rkt" text-part-text)
         "schema.rkt")

(provide (contract-out
          [lookup-entry (-> session-index? string? (or/c message? #f))]
          [children-of (-> session-index? string? (listof message?))]
          [leaf-nodes (-> session-index? (listof message?))]
          [resolve-active-leaf (-> session-index? (or/c message? #f))]
          [active-leaf (-> session-index? (or/c message? #f))]
          [get-branch (-> session-index? string? (or/c (listof message?) #f))]
          [find-common-ancestor (-> session-index? string? string? (or/c string? #f))]
          [collect-branch-entries
           (-> session-index? string? string? exact-nonnegative-integer? (listof message?))]
          [estimate-entry-tokens (-> message? exact-nonnegative-integer?)]
          [leaf-depth (-> session-index? string? (or/c exact-nonnegative-integer? #f))]))

(define (lookup-entry idx id)
  (hash-ref (session-index-by-id idx) id #f))

(define (children-of idx id)
  (hash-ref (session-index-children idx) id '()))

(define (leaf-nodes idx)
  (define children (session-index-children idx))
  (for/list ([msg (in-vector (session-index-entry-order idx))]
             #:when (null? (hash-ref children (message-id msg) '())))
    msg))

(define (resolve-active-leaf idx)
  (define leaves (leaf-nodes idx))
  (if (null? leaves)
      #f
      (last leaves)))

(define (active-leaf idx)
  (define marked-id (unbox (session-index-active-leaf-id idx)))
  (cond
    [marked-id (lookup-entry idx marked-id)]
    [else
     (define order (session-index-entry-order idx))
     (if (= (vector-length order) 0)
         #f
         (vector-ref order (sub1 (vector-length order))))]))

(define (get-branch idx entry-id)
  (define (walk-up id acc)
    (define entry (lookup-entry idx id))
    (cond
      [(not entry) #f]
      [else
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (if pid
           (walk-up pid new-acc)
           new-acc)]))
  (walk-up entry-id '()))

(define (find-common-ancestor idx id-a id-b)
  (define ancestors-a (ancestor-set idx id-a))
  (let loop ([current id-b])
    (cond
      [(not current) #f]
      [(set-member? ancestors-a current) current]
      [else
       (define msg (hash-ref (session-index-by-id idx) current #f))
       (loop (and msg (message-parent-id msg)))])))

(define (ancestor-set idx id)
  (let loop ([current id]
             [acc (set)])
    (cond
      [(not current) acc]
      [(set-member? acc current) acc]
      [else
       (define msg (hash-ref (session-index-by-id idx) current #f))
       (loop (and msg (message-parent-id msg)) (set-add acc current))])))

(define (collect-branch-entries idx old-leaf-id ancestor-id token-budget)
  (let loop ([current old-leaf-id]
             [acc '()]
             [tokens-used 0])
    (cond
      [(not current) acc]
      [(equal? current ancestor-id) acc]
      [(>= tokens-used token-budget) acc]
      [else
       (define msg (hash-ref (session-index-by-id idx) current #f))
       (cond
         [(not msg) acc]
         [else
          (define cost (estimate-entry-tokens msg))
          (loop (message-parent-id msg) (cons msg acc) (+ tokens-used cost))])])))

(define (estimate-entry-tokens msg)
  (define content (message-content msg))
  (for/sum ([part (in-list content)])
           (cond
             [(text-part? part) (quotient (string-length (text-part-text part)) 4)]
             [else 10])))

(define (leaf-depth idx entry-id)
  (define path (get-branch idx entry-id))
  (if path
      (sub1 (length path))
      #f))
