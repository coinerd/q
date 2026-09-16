#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-coordinator.rkt
;; B2a: deterministic journal-driven delivery stage machine. ONE controller
;; effect per invocation (injectable seam; the default production seam shells
;; scripts/gsd-delivery.py). Every effect re-runs the receipt blocker against
;; durable state, checks cancellation/fence/takeover, records separate
;; delivery usage, and advances the journal one stage on success. Outcomes:
;;   'ok       — exactly one effect ran and the journal advanced one stage
;;   'delivered— terminal: journal was already at the delivered stage
;;   'awaiting-review / 'retryable / 'blocked — typed stop, journal untouched
;; Model or journal completion alone never sets delivered — only controller
;; evidence does. The outer run-campaign! loop re-enters after each step.
(require rackunit
         racket/file
         racket/format
         racket/string
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  delivery-stages
                  load-delivery-journal
                  record-delivery-receipt!
                  update-delivery-journal!)
         "../extensions/gsd/delivery-coordinator.rkt")

(define (receipt-head)
  (hasheq 'repo
          "git@github.com:example/q.git"
          'branch
          "campaign/test"
          'origin
          "https://github.com/example/q.git"
          'evidence
          "docs/reports/gsd-wave-evidence/a-w0.rktd"
          'head
          (make-string 40 #\a)
          'tree
          (make-string 40 #\b)
          'attempt-id
          "attempt-1"
          'attempt-fence
          7
          'verified-at
          1789500000))

(define (call-with-campaign count proc)
  (define dir (make-temporary-file "delivery-coordinator-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display "# Plan: Delivery coordinator test\n\n## Waves\n\n" out)
        (for ([i (in-range count)])
          (fprintf out "- [Inbox] W~a: Test → waves/W~a-test.md\n" i i)
          (display-to-file "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n"
                           (build-path dir ".planning/waves" (format "W~a-test.md" i))))))
     (proc dir (migrate-campaign! dir)))
   (lambda () (delete-directory/files dir))))

(define (done-record dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
  (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                      (campaign-attempt "attempt-1" 7 0))
  (set-campaign-fence-token! rec 7)
  (set-campaign-wave-delivery-branch! (car (campaign-record-waves rec)) "campaign/test")
  (set-campaign-wave-delivery-head-sha! (car (campaign-record-waves rec)) (make-string 40 #\a))
  (persist-campaign! dir rec)
  (record-delivery-receipt! dir (campaign-plan-id rec) 0 (receipt-head))
  rec)

(define (stage-count dir plan)
  (define journal (load-delivery-journal dir plan 0))
  (and journal (hash-ref journal 'stage #f)))

(module+ test
  (test-case "one eligible controller effect per invocation; journal advances one stage at a time"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (for ([expected (in-list '("implementation-review" "implementation-pr"
                                                          "implementation-ci"
                                                          "implementation-merged"))])
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'ok)
         (check-equal? (stage-count dir plan) expected))
       (check-equal?
        (reverse calls)
        '("implementation-review" "implementation-pr" "implementation-ci" "implementation-merged")))))
  (test-case "terminal delivered stage is reported and never re-executed"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (update-delivery-journal! dir plan 0 (hasheq 'stage "delivered"))
                          (define calls 0)
                          (define (controller b p w target)
                            (set! calls (add1 calls))
                            (delivery-effect-result 'ok (hasheq 'stage target)))
                          (define outcome
                            (run-delivery-coordinator! dir plan 0 #:controller controller))
                          (check-eq? (delivery-outcome-kind outcome) 'delivered)
                          (check-equal? calls 0))))
  (test-case "awaiting-review stops immediately and never advances the journal"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'awaiting-review (hasheq 'stage target 'reason "no reviewer")))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'awaiting-review)
       (check-equal? (delivery-outcome-message outcome) "no reviewer")
       (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "retryable controller failure is a typed stop, not fabricated success"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'retryable (hasheq 'stage target 'reason "CI pending")))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'retryable)
       (check-equal? (delivery-outcome-message outcome) "CI pending")
       (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "blocker failure refuses any controller call"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define start-rec rec) ; fresh campaign: no DONE wave, no receipt authority yet
       (persist-campaign! dir start-rec)
       (record-delivery-receipt! dir (campaign-plan-id start-rec) 0 (receipt-head))
       (define count 0)
       (define (controller b p w target)
         (set! count (add1 count))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (define outcome
         (run-delivery-coordinator! dir (campaign-plan-id start-rec) 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-equal? count 0))))
  (test-case "takeover during the effect rejects the stale continuation"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (define (controller b p w target)
                            (define live (load-campaign-record dir plan))
                            (set-campaign-fence-token! live 8) ; another coordinator took over
                            (persist-campaign! dir live)
                            (delivery-effect-result 'ok (hasheq 'stage target)))
                          (define outcome
                            (run-delivery-coordinator! dir plan 0 #:controller controller))
                          (check-eq? (delivery-outcome-kind outcome) 'blocked)
                          (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "usage is recorded separately without touching implementation usage"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'ok (hasheq 'stage target 'model-calls 2 'tokens 500 'cost 0.01)))
       (void (run-delivery-coordinator! dir plan 0 #:controller controller))
       (define journal (load-delivery-journal dir plan 0))
       (check-equal? (hash-ref journal 'model-calls #f) 2)
       (check-equal? (hash-ref journal 'tokens #f) 500)
       (check-equal? (hash-ref journal 'cost #f) 0.01)
       (define durable (load-campaign-record dir plan))
       (check-false (for/or ([w (in-list (campaign-record-waves durable))])
                      (define tokens (usage-summary-total-tokens (wave-usage-summary w)))
                      (and tokens (positive? tokens))))))))
