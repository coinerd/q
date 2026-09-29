#lang racket/base
;; @covers extensions/gsd/tracker-resume.rkt
;; @speed fast
;; @suite gsd
(require rackunit
         racket/file
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/reconciliation-checkpoint.rkt"
         "../extensions/gsd/tracker-reconciliation.rkt"
         "../extensions/gsd/tracker-resume.rkt")

(define (seed-wave! dir status)
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file
   (build-path dir ".planning" "PLAN.md")
   (lambda () (displayln "# Plan: Resume tracker\n\n- [Inbox] W0: Evidence → waves/W0-e.md"))
   #:exists 'replace)
  (with-output-to-file
   (build-path dir ".planning" "waves" "W0-e.md")
   (lambda ()
     (displayln
      "# Wave 0\nStatus: Inbox\n\n## Files\n\n- File: q/src/a.rkt\n\n## Verify\n\nraco test .\n\n## Done\n\nmeasured"))
   #:exists 'replace)
  (define rec (migrate-campaign! dir))
  (set-campaign-wave-status! (car (campaign-record-waves rec)) status)
  (persist-campaign! dir rec)
  rec)

(test-case "a restart after DONE but before tracker writes retries the targeted wave"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rec (seed-wave! dir 'done))
     (define calls 0)
     (define (resolver _dir _plan _wave)
       (lambda (_base _plan _wave #:delivery-reader _reader)
         (set! calls (add1 calls))
         (tracker-reconciliation-result 'reconciled #f (make-string 40 #\a) '(board close))))
     ;; This fixture injects only the reconciler; the production resolver
     ;; additionally verifies receipt, handoff and authenticated readback.
     (define (target? _base plan wave)
       (and (equal? plan (campaign-plan-id rec)) (zero? wave)))
     (define (resume)
       (resume-tracker-reconciliation! dir
                                       rec
                                       #:target? target?
                                       #:resolver resolver
                                       #:delivery-reader (lambda _ (error 'test "fixture only"))))
     (check-equal? calls 0) ; process died after DONE before the checkpoint
     (define results (resume))
     (check-equal? (length results) 1)
     (check-equal? (reconciliation-checkpoint-result-status (car results)) 'reconciled)
     (check-equal? calls 1)
     ;; Replaying a completed pass is allowed: board Status=Done and issue
     ;; close are externally idempotent. Never rely on an untrusted marker.
     (resume)
     (check-equal? calls 2))
   (lambda () (delete-directory/files dir))))

(test-case "a non-DONE wave or absent live binding never constructs the port"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rec (seed-wave! dir 'awaiting-delivery))
     (define (unexpected . _)
       (error 'test "tracker called without DONE and binding"))
     (check-equal?
      (resume-tracker-reconciliation! dir rec #:target? (lambda _ #t) #:resolver unexpected)
      '())
     (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
     (persist-campaign! dir rec)
     (check-equal?
      (resume-tracker-reconciliation! dir rec #:target? (lambda _ #f) #:resolver unexpected)
      '()))
   (lambda () (delete-directory/files dir))))
