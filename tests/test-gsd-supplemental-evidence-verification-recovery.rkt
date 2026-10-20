#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 180
;; @covers extensions/gsd/supplemental-evidence-controller.rkt
(require rackunit
         json
         "helpers/supplemental-go-fixture.rkt"
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/events.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-operator.rkt"
         "../extensions/gsd/supplemental-evidence-publication.rkt"
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-verification-repair-retries))

(define (approve-event event payload)
  (when (equal? event "supplemental-evidence.approval-requested")
    (thread (lambda ()
              (approval-decide! (hash-ref payload 'request-id)
                                (hash-ref payload 'proposal-sha256) #t)))))

(define (with-bus bus thunk)
  (define old (unbox gsd-event-bus-box))
  (dynamic-wind (lambda () (set-gsd-event-bus! bus)) thunk
                (lambda () (set-gsd-event-bus! old))))

(module+ test
  (test-case "completed verifying admission cannot launch C1 on production resume"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define grant (hash-ref (load-supplemental-pointer root plan 1) 'task-approval))
       (define snapshot-path
         (build-path (supplemental-fixture-private fx) "supplemental-operations"
                     (string-append (hash-ref grant 'sha256) ".json")))
       (define executions 0)
       (define dispatches 0)
       (define interrupted? #f)
       ;; Escape instead of throwing: process loss must not run the
       ;; controller's failure handler and turn verifying into blocked.
       (let/ec interrupt
         (with-bus
          (lambda (event _)
            (when (eq? event 'gsd.verification.started)
              (set! executions (add1 executions))))
          (lambda ()
            (parameterize
                ([current-supplemental-operator-publisher approve-event]
                 [current-gsd-wave-verification-repair-retries 0]
                 [current-supplemental-journal-crash-cut
                  (lambda (at)
                    ;; AFTER snapshot + intent deletion, not a pending WAL.
                    ;; Read bytes directly: the hook holds the owner lock.
                    (when (and (eq? at 'intent-cleared)
                               (equal? (hash-ref (call-with-input-file snapshot-path read-json) 'state)
                                       "verifying"))
                      (set! interrupted? #t)
                      (interrupt (void))))])
              (run-campaign-wave root (load-campaign-record root plan) 1
                                 #:fence-token 8 #:timeout-sec 90
                                 #:timeout-retries 0 #:no-change-retries 0
                                 #:stall-soft-limit #f #:stall-hard-limit #f
                                 #:isolate? #f #:advance-override? #t
                                 #:runner (lambda (_)
                                            (set! dispatches (add1 dispatches))
                                            (fixture-author-commit! (current-directory))
                                            'ok)
                                 #:verifier (lambda (_) (error 'fixture "ordinary verifier called")))))))
       (check-true interrupted?)
       (check-equal? dispatches 1)
       (check-equal? executions 0)
       (check-equal? (hash-ref (load-supplemental-operation grant) 'state) "verifying")
       (check-false (load-supplemental-staged-evidence root plan 1))
       ;; Observe real execution, not a mocked admission. On RED escape at
       ;; the forbidden start to bound the gate before publication/probing.
       (let/ec forbidden-launch
         (with-bus
          (lambda (event _)
            (when (eq? event 'gsd.verification.started)
              (set! executions (add1 executions))
              (forbidden-launch (void))))
          (lambda ()
            ;; Repeat resume: a pause must never re-arm the spent admission.
            (for ([_ (in-range 2)])
              (define-values (kind detail) (advance-supplemental-evidence! root plan 1))
              (check-equal? kind 'paused)
              (check-regexp-match #rx"ambiguous.*verification|verification.*ambiguous" detail)))))
       (check-equal? executions 0 "resume must not invoke the capture verifier again")
       (check-equal? (hash-ref (load-supplemental-operation grant) 'state) "verifying")
       (check-false (load-supplemental-staged-evidence root plan 1))))))
