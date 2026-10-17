#lang racket
;; Genuine owned receipt + running successor with an owner-committed test delta.
(require rackunit
         "delivery-fixtures.rkt"
         "private-fixture-templates.rkt"
         "../../extensions/gsd/author-scope.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/delivery-coordinator.rkt"
         "../../extensions/gsd/delivery-finalize.rkt"
         "../../extensions/gsd/evidence-author-journal.rkt"
         "../../extensions/gsd/evidence-readiness.rkt"
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/verification-job.rkt"
         (only-in "../../extensions/gsd/go-orchestrator.rkt" run-campaign-wave))
(provide (struct-out lost-author) with-lost-author put! commit! git-output journal)
(struct lost-author (root plan cwd old) #:transparent)
(define (put! path text)
  (make-parent-directory* path)
  (display-to-file text path #:exists 'truncate))
(define (git-output repo . args)
  (string-trim
   (with-output-to-string
     (lambda ()
       (parameterize ([current-directory repo])
         (unless (apply system* (find-executable-path "git") args)
           (error 'fixture "git failed: ~s" args)))))))
(define (commit! repo path text)
  (put! (build-path repo path) text)
  (git-quiet! repo "add" "--" path)
  (git-quiet! repo "commit" "-qm" "owner test-author delta"))
(define (journal f)
  (load-delivery-journal (lost-author-root f) (lost-author-plan f) 0))
(define (with-lost-author proc)
  (define root (make-temporary-file "lost-author-finish-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define repo (build-path root "q"))
     (make-directory repo)
     (git-quiet! repo "init" "-q")
     (hermetic-identity! repo)
     (define remote (build-path root "origin.git"))
     (git-quiet! repo "init" "--bare" "-q" (path->string remote))
     (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
     (git-quiet! repo "config" (string-append "url.file://" (path->string remote) ".insteadOf")
                 "https://github.com/example/q.git")
     (put! (build-path repo ".gitignore") "compiled/\n")
     (put! (build-path repo "source.rkt")
           "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
     (git-quiet! repo "add" ".")
     (git-quiet! repo "commit" "-qm" "baseline")
     (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
     (git-quiet! repo "update-ref" "refs/remotes/origin/main" "HEAD")
     (make-directory* (build-path root ".planning" "waves"))
     (write-plan! root 0 "Checkpoint" "checkpoint")
     (write-wave-doc! root 0 "checkpoint"
                      '("q/source.rkt" "q/tests/probe.rkt" "q/tests/second.rkt")
                      "raco test tests/probe.rkt && raco test tests/second.rkt")
     (define record (migrate-campaign! root))
     (set-campaign-record-execution-policy! record #f)
     (persist-campaign! root record)
     (define plan (campaign-plan-id record))
     (define registry (make-verification-registry))
     (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                    [current-gsd-verification-registry registry])
       (define cwd #f)
       (run-campaign-wave
        root record 0 #:fence-token 1 #:timeout-sec 180 #:timeout-retries 0
        #:no-change-retries 0 #:isolate? #t #:stall-soft-limit #f #:stall-hard-limit #f
        #:runner
        (lambda (_)
          (set! cwd (current-directory))
          (put! (build-path cwd "source.rkt")
                "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
          (put! (build-path cwd "tests/probe.rkt")
                "#lang racket/base\n(require rackunit \"../source.rkt\")\n(module+ test (check-equal? (answer) 1))\n")
          (put! (build-path cwd "tests/second.rkt")
                "#lang racket/base\n(require rackunit)\n(module+ test (check-true #t))\n")
          'ok)
        #:verifier (lambda (_)
                     (define check (check-verify-command root 0 #f))
                     (delivery-verification (cadr check) (list check) "actual full Verify")))
       (define old (hash-ref (load-delivery-journal root plan 0) 'receipt))
       (check-not-false (load-receipt-owned-proof old root plan 0))
       (run-coordinator-checkpoint
        root plan (car (campaign-record-waves (load-campaign-record root plan))) "repair"
        (lambda (dir id wave)
          (define result (default-delivery-controller dir id wave "implementation-review"))
          (cons (delivery-effect-result-kind result) (format "~s" (delivery-effect-result-data result)))))
       (unless (evidence-author-repair-pending? (load-delivery-journal root plan 0))
         (reserve-evidence-author-repair! root plan 0 (current-evidence-context root plan 0)
                                          (frozen-author-files root plan 0) #:guard (lambda () #t)))
       ;; Durable existing successor; no inference about which model phases ran.
       (define rec (load-campaign-record root plan))
       (set-campaign-fence-token! rec 2)
       (begin-attempt! rec 0 2)
       (set-campaign-wave-status! (car (campaign-record-waves rec)) 'awaiting-delivery)
       (persist-campaign! root rec)
       (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
       (start-evidence-author-repair! root plan 0 (campaign-attempt-id attempt) 2 #:guard (lambda () #t))
       (commit! cwd "tests/probe.rkt"
                (string-append
                 "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
                 "(define evidence-suite (test-suite \"regression\" (test-case \"answer\" (check-equal? (answer) 1))))\n"
                 "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
       (dynamic-wind void
                     (lambda () (proc (lost-author root plan cwd old)))
                     (lambda () (verification-close-registry! registry)))))
   (lambda () (delete-directory/files root))))
