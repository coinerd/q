#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/delivery-finalize.rkt
;; @covers extensions/gsd/delivery-coordinator.rkt
(require rackunit
         racket/file
         json
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/policy.rkt")
(define manifest (make-campaign-manifest 1 "w0" '() '() "constraints"))
(define plan (campaign-manifest-hash manifest))
(define receipt
  (hasheq 'repo
          "/repo"
          'branch
          "campaign/w0"
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "genuine Verify"
          'attempt-id
          "author-1"
          'attempt-fence
          4))
(define rejection
  (hash-set* receipt
             'findings
             '("Fix quoted delimiters: a=b; findings: exact\nsecond line")
             'postmerge
             #f))
(define (fixture f)
  (define root (make-temporary-file "auto-review-integration-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rec
       (make-campaign-record
        plan
        manifest
        (list (make-campaign-wave 0 "w0" 'awaiting-delivery 1 (campaign-attempt "author-1" 4 0)))
        #f
        4
        'test
        0
        0))
     (persist-campaign! root rec)
     (record-delivery-receipt! root plan 0 receipt)
     (f root rec))
   (lambda () (delete-directory/files root))))
(define (checkpoint root rec data)
  (run-coordinator-checkpoint root
                              plan
                              (car (campaign-record-waves rec))
                              "pending"
                              (lambda (_root _plan _wave)
                                (delivery-outcome 'review-rejected (review-rejected-message data)))
                              #:on-cancel (lambda (_) 'cancelled)
                              #:on-advance (lambda () 'retry)
                              #:on-blocked (lambda (_) 'blocked)))
(module+ test
  (test-case "real coordinator rejection checkpoint persists ordinary retry plus verbatim findings"
    (fixture (lambda (root rec)
               (check-equal? (checkpoint root rec rejection) 'retry)
               (define live (load-campaign-record root plan))
               (check-equal? (campaign-wave-status (car (campaign-record-waves live))) 'failed)
               (define journal (load-delivery-journal root plan 0))
               (check-equal? (hash-ref journal 'review-rejection-count 0) 1)
               (check-equal? (hash-ref (hash-ref journal 'review-rejection (hash)) 'findings #f)
                             (hash-ref rejection 'findings)))))
  (test-case "cancel, exhausted retry, stale identity, and postmerge never reenter implementation"
    (for ([mode '(cancel exhausted stale postmerge malformed)])
      (fixture
       (lambda (root rec)
         (when (eq? mode 'cancel)
           (set-campaign-cancellation! rec (make-campaign-cancellation "operator" 0))
           (persist-campaign! root rec))
         (define data
           (case mode
             [(stale) (hash-set rejection 'head (make-string 40 #\d))]
             [(postmerge) (hash-set rejection 'postmerge #t)]
             [(malformed) (hash-set rejection 'findings '())]
             [else rejection]))
         (define result
           (parameterize ([current-gsd-wave-review-rejection-retries (if (eq? mode 'exhausted) 0 1)])
             (checkpoint root rec data)))
         (check-equal? result (if (eq? mode 'cancel) 'cancelled 'blocked))
         (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count 0) 0)))))
  (test-case "journal-first crash window rederives persisted retry without a second reviewer"
    (fixture (lambda (root rec)
               (record-review-rejection! root
                                         plan
                                         0
                                         #:findings (hash-ref rejection 'findings)
                                         #:expected-attempt-id "author-1"
                                         #:expected-fence 4
                                         #:max-rejection-retries 1)
               (check-equal? (run-coordinator-checkpoint
                              root
                              plan
                              (car (campaign-record-waves rec))
                              "pending"
                              (lambda (_d _p _w)
                                (error 'test "durable rejection must replay without dispatch"))
                              #:on-advance (lambda () 'retry)
                              #:on-blocked (lambda (_) 'blocked))
                             'retry)
               (check-equal?
                (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan))))
                'failed)
               (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count)
                             1))))
  (test-case "infrastructure failure does not consume rejection budget"
    (fixture (lambda (root rec)
               (check-equal? (run-coordinator-checkpoint root
                                                         plan
                                                         (car (campaign-record-waves rec))
                                                         "pending"
                                                         (lambda (_d _p _w)
                                                           (delivery-outcome 'review-infra "timeout"))
                                                         #:on-advance (lambda () 'retry)
                                                         #:on-blocked (lambda (_) 'blocked))
                             'blocked)
               (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count 0)
                             0)))))
