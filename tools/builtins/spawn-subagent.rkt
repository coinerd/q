#lang racket/base

;; Subagent spawning orchestration — thin re-export facade.
;; All implementation lives in spawn-execution.rkt and spawn-coordinator.rkt.

(require (only-in "spawn-subagent-helpers.rkt" extract-text-summary SUBAGENT-SUMMARY-MAX-CHARS)
         "spawn-coordinator.rkt")

(provide (all-from-out "spawn-coordinator.rkt"))

(module+ test
  (require rackunit)
  (provide extract-text-summary
           SUBAGENT-SUMMARY-MAX-CHARS)
  ;; Truncation limit test
  (check-equal? SUBAGENT-SUMMARY-MAX-CHARS 4000)
  (check-true
   (>= (string-length (extract-text-summary (list (hasheq 'type "text" 'text (make-string 500 #\X)))))
       497)))
