#lang typed/racket

;; util/loop-result.rkt — Loop result struct
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Migrated to Typed Racket (W23) per ADR 0014.
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from the TR boundary system. Struct
;; constructors and accessors enforce field types at call sites.

(provide loop-result
         loop-result?
         loop-result-messages
         loop-result-termination-reason
         loop-result-metadata
         make-loop-result)

;; ============================================================
;; Loop result struct
;; ============================================================

(struct loop-result ([messages : (Listof Any)]
                     [termination-reason : (U Symbol #f)]
                     [metadata : (HashTable Symbol Any)])
  #:transparent)

(: make-loop-result (-> (Listof Any) (U Symbol #f) (HashTable Symbol Any) loop-result))
(define (make-loop-result messages termination-reason metadata)
  (loop-result messages termination-reason metadata))
