#lang racket/base
;; util/hash-utils.rkt — Deep hash access utilities (W4)
;;
;; STABILITY: public
;;
;; Adds hash-ref* for deep, path-based hash access.
;; This is the generic counterpart of runtime/settings-query.rkt's
;; setting-ref* (which reads from the q-settings struct); hash-ref*
;; works on any nested hash structure.
;;
;; W4 (v0.99.42 inventory §3.1): Introduced to replace deep
;; (hash-ref (hash-ref ...)) chains for settings → model → provider
;; style config paths. Additive only — existing hash-ref call sites
;; are untouched.

(require racket/contract)

(provide (contract-out
          [hash-ref* (->* (hash? (listof (or/c symbol? string?)))
                          (any/c)
                          any/c)]))

;; hash-ref* : hash? (listof key?) [default any/c] → any/c
;; Deep hash access: (hash-ref* h '(a b c))
;;   ≡ (hash-ref (hash-ref (hash-ref h 'a) 'b) 'c)
;; If any step in the path is missing — or an intermediate value is
;; not a hash — returns default (which defaults to #f), matching the
;; setting-ref* semantics used across the settings query layer.
(define (hash-ref* h keys [default #f])
  (for/fold ([acc h]) ([k (in-list keys)])
    (if (hash? acc)
        (hash-ref acc k default)
        default)))
