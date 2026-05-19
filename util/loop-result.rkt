#lang racket/base

;; util/loop-result.rkt — Loop result struct
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).

(provide loop-result
         loop-result?
         loop-result-messages
         loop-result-termination-reason
         loop-result-metadata
         make-loop-result)

(struct loop-result (messages termination-reason metadata) #:transparent)

(define (make-loop-result messages termination-reason metadata)
  (loop-result messages termination-reason metadata))
