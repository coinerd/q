#lang racket/base

;; util/loop-result.rkt — Loop result struct
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).

(require racket/contract)

(provide (contract-out [loop-result? (-> any/c boolean?)]
                       [loop-result-messages (-> loop-result? any/c)]
                       [loop-result-termination-reason (-> loop-result? any/c)]
                       [loop-result-metadata (-> loop-result? any/c)]
                       [make-loop-result (-> any/c any/c any/c loop-result?)])
         loop-result)

(struct loop-result (messages termination-reason metadata) #:transparent)

(define (make-loop-result messages termination-reason metadata)
  (loop-result messages termination-reason metadata))
