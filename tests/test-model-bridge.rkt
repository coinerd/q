#lang racket/base

;; @speed fast
;; @suite arch

(require rackunit
         rackunit/text-ui
         "../tools/model-bridge.rkt")

(define suite
  (test-suite "tool model bridge"
    (test-case "provider identity is available through the layer facade"
      (define response (make-model-response '() (hasheq) "test-model" 'stop))
      (define provider (make-mock-provider response #:name "bound-provider"))
      (check-equal? (provider-name provider) "bound-provider"))))

(exit (run-tests suite))
