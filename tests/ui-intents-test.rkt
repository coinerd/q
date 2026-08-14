#lang racket/base

;; Tests for q/ui-core/ui-intents.rkt

(require rackunit
         "../ui-core/ui-intents.rkt")

(define i (make-toggle-detail-intent))
(check-true (toggle-detail-intent? i))
(check-equal? (ui-intent-kind i) 'ui.transcript.toggle-detail)
(check-equal? (ui-intent-target i) #f)

(define i2 (make-toggle-detail-intent "art-42"))
(check-equal? (ui-intent-target i2) "art-42")
(check-equal? (ui-intent-kind i2) 'ui.transcript.toggle-detail)

(check-true (ui-intent? i))
(check-false (ui-intent? 'not-an-intent))
