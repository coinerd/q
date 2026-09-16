#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/go-orchestrator.rkt
;; @covers extensions/gsd/delivery-handoff.rkt
(require rackunit
         racket/file
         racket/format
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         (only-in "../extensions/gsd/wave-completion.rkt" load-outbox)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign!
                  campaign-result-status
                  campaign-result-message))

(define (call-with-campaign count proc)
  (define dir (make-temporary-file "delivery-runtime-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display "# Plan: Delivery test\n\n## Waves\n\n" out)
        (for ([i (in-range count)])
          (fprintf out "- [Inbox] W~a: Test → waves/W~a-test.md\n" i i)
          (display-to-file "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n"
                           (build-path dir ".planning/waves" (format "W~a-test.md" i))))))
     (proc dir (migrate-campaign! dir)))
   (lambda () (delete-directory/files dir))))

(define (delivered _ plan idx)
  (hasheq 'status "delivered" 'plan-id plan 'wave idx 'merge-sha (make-string 40 #\a)))
(define (pending _ _p _w)
  (hasheq 'status "delivery-pending" 'reason "implementation PR pending"))

(module+ test
  (test-case "shared-checkout Verify records the exact committed provenance without rerunning implementation"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define repo (build-path dir "q"))
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (git-quiet! repo "checkout" "-b" "campaign/test")
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (display-to-file "payload" (build-path repo "payload"))
       (git-quiet! repo "add" "payload")
       (git-quiet! repo "commit" "-qm" "verified implementation")
       (define runs 0)
       (run-campaign! dir
                      rec
                      #:runner (lambda (_)
                                 (set! runs (add1 runs))
                                 'ok)
                      #:verifier (lambda (_) #t)
                      #:delivery-reader pending)
       (define journal (load-delivery-journal dir (campaign-plan-id rec) 0))
       (check-true (hash? journal))
       (check-equal? (hash-ref (hash-ref journal 'receipt) 'branch) "campaign/test")
       (run-campaign! dir
                      rec
                      #:runner (lambda (_)
                                 (set! runs (add1 runs))
                                 'ok)
                      #:delivery-reader pending)
       (check-equal? runs 1)
       (check-equal? journal (load-delivery-journal dir (campaign-plan-id rec) 0)))))
  (test-case "verified first wave stops before successor; resume only runs successor"
    (call-with-campaign
     2
     (lambda (dir rec)
       (define runs '())
       (define (runner idx)
         (set! runs (cons idx runs))
         'ok)
       (define stopped
         (run-campaign! dir rec #:runner runner #:verifier (lambda (_) #t) #:delivery-reader pending))
       (check-eq? (campaign-result-status stopped) 'wave-blocked)
       (check-equal? runs '(0))
       (define plan (campaign-plan-id rec))
       (define saved (load-campaign-record dir plan))
       (check-eq? (campaign-wave-status (car (campaign-record-waves saved))) 'done)
       (define attempt-count (campaign-wave-attempt-count (car (campaign-record-waves saved))))
       (define resumed
         (run-campaign! dir
                        saved
                        #:runner runner
                        #:verifier (lambda (_) #t)
                        #:delivery-reader delivered))
       (check-eq? (campaign-result-status resumed) 'campaign-complete)
       (check-equal? runs '(1 0))
       (check-equal? attempt-count
                     (campaign-wave-attempt-count
                      (car (campaign-record-waves (load-campaign-record dir plan)))))
       (check-eq? (hash-ref (call-with-input-file (delivery-handoff-path dir plan 0) read) 'status)
                  'delivered))))
  (test-case "final wave cannot complete campaign without delivery; resume does not rerun or duplicate outbox"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define count 0)
       (define (runner _)
         (set! count (add1 count))
         'ok)
       (check-eq?
        (campaign-result-status
         (run-campaign! dir rec #:runner runner #:verifier (lambda (_) #t) #:delivery-reader pending))
        'wave-blocked)
       (define plan (campaign-plan-id rec))
       (define ledger (file->bytes (delivery-handoff-path dir plan 0)))
       (define outbox (load-outbox dir plan))
       (check-eq?
        (campaign-result-status (run-campaign! dir rec #:runner runner #:delivery-reader pending))
        'wave-blocked)
       (check-equal? ledger (file->bytes (delivery-handoff-path dir plan 0)))
       (check-eq?
        (campaign-result-status (run-campaign! dir rec #:runner runner #:delivery-reader delivered))
        'campaign-complete)
       (check-equal? count 1)
       (check-equal? outbox (load-outbox dir plan)))))
  (test-case "wrong campaign/wave or missing/short SHA cannot satisfy final delivery"
    (for ([mutate (in-list (list (lambda (p) (hash-set p 'plan-id (make-string 64 #\b)))
                                 (lambda (p) (hash-set p 'wave 9))
                                 (lambda (p) (hash-set p 'merge-sha "abc"))
                                 (lambda (p) (hash-remove p 'merge-sha))))])
      (call-with-campaign 1
                          (lambda (dir rec)
                            (define result
                              (run-campaign! dir
                                             rec
                                             #:runner (lambda (_) 'ok)
                                             #:verifier (lambda (_) #t)
                                             #:delivery-reader
                                             (lambda (b p w) (mutate (delivered b p w)))))
                            (check-eq? (campaign-result-status result) 'wave-blocked)))))
  (test-case "cancellation/fence replacement during authenticated readback wins over completion"
    (for ([kind '(cancel fence disappear)])
      (call-with-campaign
       1
       (lambda (dir rec)
         (define result
           (run-campaign!
            dir
            rec
            #:runner (lambda (_) 'ok)
            #:verifier (lambda (_) #t)
            #:delivery-reader
            (lambda (b p w)
              (define current (load-campaign-record b p))
              (case kind
                [(cancel)
                 (set-campaign-cancellation! current (make-campaign-cancellation "stop" 1))
                 (persist-campaign! b current)]
                [(fence)
                 (set-campaign-fence-token! current (add1 (campaign-fence-token current)))
                 (persist-campaign! b current)]
                [(disappear)
                 (delete-file (build-path b ".planning/campaigns" (string-append p ".rktd")))])
              (delivered b p w))))
         (check-eq? (campaign-result-status result) 'wave-cancelled)))))
  (test-case "a later runner mutation invalidates prior loop's cached synchronized proof"
    (call-with-campaign 2
                        (lambda (dir rec)
                          (define valid? #t)
                          (define result
                            (run-campaign! dir
                                           rec
                                           #:verifier (lambda (_) #t)
                                           #:runner (lambda (idx)
                                                      (when (= idx 1)
                                                        (set! valid? #f))
                                                      'ok)
                                           #:delivery-reader (lambda (b p w)
                                                               (if valid?
                                                                   (delivered b p w)
                                                                   (pending b p w)))))
                          (check-eq? (campaign-result-status result) 'wave-blocked))))
  (test-case "handoff path rejects symlink ancestors and invalid wave indices"
    (define dir (make-temporary-file "handoff-links-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (check-exn exn:fail? (lambda () (delivery-handoff-path dir (make-string 64 #\a) "../escape")))
       (make-directory* (build-path dir "outside"))
       (make-file-or-directory-link (build-path dir "outside") (build-path dir ".planning"))
       (check-exn exn:fail? (lambda () (delivery-handoff-path dir (make-string 64 #\a) 0))))
     (lambda () (delete-directory/files dir)))))
