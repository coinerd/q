#lang racket/base

;; util/loop-result.rkt — Loop result struct
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).

(require racket/contract)

(provide (contract-out [loop-result? (-> any/c boolean?)]
                       [loop-result-messages (-> loop-result? list?)]
                       [loop-result-termination-reason (-> loop-result? (or/c symbol? #f))]
                       [loop-result-metadata (-> loop-result? hash?)]
                       [make-loop-result (-> list? (or/c symbol? #f) hash? loop-result?)])
         loop-result)

(struct loop-result (messages termination-reason metadata) #:transparent)

(define (make-loop-result messages termination-reason metadata)
  (loop-result messages termination-reason metadata))
