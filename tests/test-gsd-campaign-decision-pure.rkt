#lang racket/base

;; @covers extensions/gsd/campaign-decision.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit

;; Pure decision coverage; durable state, locks and interpreter effects remain
;; in test-gsd-campaign-decision.rkt.
(require rackunit
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave
                  make-campaign-record
                  make-campaign-cancellation)
         "../extensions/gsd/campaign-decision.rkt")

(define plan-id (make-string 64 #\a))
(define sha (make-string 40 #\b))
(define (wave index status)
  (make-campaign-wave index (format "w~a" index) status 0 #f))
(define (record waves [cancelled? #f])
  (make-campaign-record plan-id
                        (make-campaign-manifest 1 "fixture" '() '() "constraints")
                        waves
                        (and cancelled? (make-campaign-cancellation "stop" 0))
                        0
                        'test
                        0
                        0))
(define (views rec
               #:stale? [stale? #f]
               #:pending [pending #f]
               #:awaiting [awaiting (hash)]
               #:lane [lane #f]
               #:message [message "pending"]
               #:coordinator? [coordinator? #f])
  (campaign-views rec '() stale? (campaign-delivery-view pending awaiting) lane message coordinator?))
(define (lane #:author [author #f]
              #:resumable [resumable #f]
              #:retained [retained #f]
              #:repair [repair #f]
              #:running [running #f]
              #:review [review #f])
  (campaign-lane-view 0 #f author resumable retained repair running review))

(test-case "complete, launch and authenticated finalization"
  (define complete (decide-campaign-action (views (record (list (wave 0 'done) (wave 1 'deferred))))))
  (check-equal? (list (campaign-decision-kind complete) (campaign-decision-code complete))
                '(complete campaign-complete))
  (define launch (decide-campaign-action (views (record (list (wave 0 'done) (wave 1 'pending))))))
  (check-equal? (list (campaign-decision-kind launch)
                      (campaign-decision-wave-index launch)
                      (campaign-decision-code launch))
                '(launch 1 wave-launch))
  (define final
    (decide-campaign-action (views (record (list (wave 0 'awaiting-delivery) (wave 1 'pending)))
                                   #:awaiting (hash 0 sha))))
  (check-equal? (list (campaign-decision-kind final)
                      (campaign-decision-wave-index final)
                      (campaign-decision-code final)
                      (campaign-decision-merge-sha final))
                (list 'finalize 0 'delivery-ready sha)))

(test-case "stale readback and cancellation precede pending and proof"
  (define rec (record (list (wave 0 'awaiting-delivery) (wave 1 'pending)) #t))
  (define (decide stale?)
    (decide-campaign-action (views rec
                                   #:stale? stale?
                                   #:pending (cons 0 "reason")
                                   #:awaiting (hash 0 sha)
                                   #:coordinator? #t)))
  (check-eq? (campaign-decision-code (decide #t)) 'campaign-stale-readback)
  (check-eq? (campaign-decision-code (decide #f)) 'campaign-cancelled))

(test-case "pending delivery takes precedence over proof and launch"
  (define d
    (decide-campaign-action (views (record (list (wave 0 'awaiting-delivery) (wave 1 'pending)))
                                   #:pending (cons 0 "unbound")
                                   #:awaiting (hash 0 sha))))
  (check-equal? (list (campaign-decision-kind d)
                      (campaign-decision-wave-index d)
                      (campaign-decision-route d)
                      (campaign-decision-code d))
                '(block 0 none delivery-pending))
  (check-false (campaign-decision-merge-sha d)))

(test-case "only first awaiting wave is considered for finalization"
  (define rec (record (list (wave 0 'awaiting-delivery) (wave 1 'awaiting-delivery))))
  (define d (decide-campaign-action (views rec #:awaiting (hash 1 sha))))
  (check-not-eq? (campaign-decision-kind d) 'finalize))

(test-case "lane routing precedence and coordinator capability"
  (for ([entry
         (in-list
          (list (list (lane #:author #t #:resumable #t #:retained #t #:repair #t #:review 'rejected)
                      #f
                      'resume
                      'supplemental-author
                      'supplemental-author-pending)
                (list (lane #:resumable #t #:retained #t)
                      #f
                      'resume
                      'supplemental-checkpoint
                      'supplemental-resume)
                (list (lane #:retained #t #:repair #t)
                      #t
                      'block
                      'supplemental-retained
                      'supplemental-retained)
                (list (lane #:repair #t #:running #t #:review 'rejected)
                      #t
                      'resume
                      'author-publication-recovery
                      'evidence-author-recovery)
                (list (lane #:repair #t #:review 'rejected)
                      #t
                      'resume
                      'author-permission
                      'evidence-author-permission)
                (list (lane #:review 'rejected) #t 'resume 'review-replay 'review-replay)
                (list (lane #:repair #t #:review 'rejected) #f 'block 'none 'delivery-pending)
                (list #f #t 'resume 'coordinator-step 'coordinator-step)
                (list #f #f 'block 'none 'delivery-pending)))])
    (define d (decide-lane-action (car entry) 7 "detail" (cadr entry)))
    (check-equal? (list (campaign-decision-kind d)
                        (campaign-decision-route d)
                        (campaign-decision-code d)
                        (campaign-decision-wave-index d))
                  (list (caddr entry) (cadddr entry) (list-ref entry 4) 7))))

(test-case "operator text does not affect routing and fallback messages are supplied"
  (define l (lane #:retained #t))
  (define a (decide-lane-action l 0 "coordinator-step" #t))
  (define b (decide-lane-action l 0 #f #t))
  (check-eq? (campaign-decision-route a) (campaign-decision-route b))
  (check-eq? (campaign-decision-code b) 'supplemental-retained)
  (check-equal? (campaign-decision-message b) "retained supplemental task is not launchable")
  (check-equal? (campaign-decision-message (decide-lane-action #f 0 #f #f))
                "campaign is blocked without an actionable route"))
