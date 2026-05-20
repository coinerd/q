#lang racket/base
;; util/hash-helpers.rkt — Hash utility functions (T3-6)
;; STABILITY: evolving
;;
;; Reusable hash operations extracted from model-registry.rkt.
;; Provides flex-ref (flexible key lookup) and normalize-keys
;; (string-key normalization).

(require racket/contract)

(provide (contract-out [flex-ref (->* (hash? (or/c string? symbol?)) (any/c) any/c)]
                       [normalize-keys (-> hash? hash?)]
                       [key->string (-> (or/c string? symbol?) string?)]))

;; key->string : (or/c string symbol) -> string
;; Convert a symbol key to string; pass strings through.
(define (key->string k)
  (if (symbol? k)
      (symbol->string k)
      k))

;; normalize-keys : hash -> hash
;; Normalize all keys in a hash to strings.
(define (normalize-keys h)
  (for/hash ([(k v) (in-hash h)])
    (values (key->string k) v)))

;; flex-ref : hash (or/c string symbol) [default] -> any
;; Flexible hash-ref: tries both string and symbol keys.
;; Handles JSON configs with string keys vs Racket symbol keys.
(define (flex-ref h key [default #f])
  (define str-key
    (if (symbol? key)
        (symbol->string key)
        key))
  (define sym-key
    (if (string? key)
        (string->symbol key)
        key))
  (cond
    [(hash-has-key? h str-key) (hash-ref h str-key)]
    [(hash-has-key? h sym-key) (hash-ref h sym-key)]
    [(procedure? default) (default)]
    [else default]))
