#lang racket/base
;; STABILITY: internal
;; Add reviewer usage to cumulative wave spend without replacing author usage.
(require "campaign-state.rkt"
         "campaign-repository.rkt"
         "campaign-budgets.rkt")
(provide account-review-usage!)
(define (account-review-usage! root plan wave result)
  (define rec (load-campaign-record root plan))
  (unless rec
    (error 'review-usage "campaign disappeared during reviewer accounting"))
  (define w
    (for/first ([w (in-list (campaign-record-waves rec))]
                #:when (= wave (campaign-wave-index w)))
      w))
  (unless w
    (error 'review-usage "review wave disappeared"))
  (define a (campaign-wave-current-attempt w))
  (unless a
    (error 'review-usage "review attempt disappeared"))
  (define prior
    (list (campaign-attempt-input-tokens a)
          (campaign-attempt-output-tokens a)
          (campaign-attempt-total-tokens a)
          (campaign-attempt-cost-usd a)
          (campaign-attempt-usage-source a)))
  (stamp-wave-usage! rec wave (loop-result->usage-datum result))
  (for ([setter (in-list (list set-campaign-attempt-input-tokens!
                               set-campaign-attempt-output-tokens!
                               set-campaign-attempt-total-tokens!
                               set-campaign-attempt-cost-usd!
                               set-campaign-attempt-usage-source!))]
        [value (in-list prior)])
    (setter a value))
  (persist-campaign! root rec)
  (pause-campaign-if-over-budget! root plan))
