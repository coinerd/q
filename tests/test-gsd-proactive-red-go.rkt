#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 240
;; @covers extensions/gsd/go-orchestrator.rkt extensions/gsd/red-checkpoint.rkt
(require rackunit
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/author-execution.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave)
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")
(module+ test
  (test-case "fresh production policy schedules test-author, owned RED, implementation, then full declared Verify"
    (define root (make-temporary-file "proactive-go-~a" 'directory))
    (define repo (build-path root "q"))
    (define phases '())
    (define (put path text)
      (make-directory* (path-only path))
      (display-to-file text path #:exists 'truncate))
    (dynamic-wind
     void
     (lambda ()
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (define remote (build-path root "origin.git"))
       (make-directory remote)
       (git-quiet! remote "init" "--bare" "-q")
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo
                   "config"
                   (string-append "url.file://" (path->string remote) ".insteadOf")
                   "https://github.com/example/q.git")
       (put (build-path repo ".gitignore") "compiled/\n")
       (put (build-path repo "source.rkt")
            "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
       (git-quiet! repo "add" ".")
       (git-quiet! repo "commit" "-qm" "unfixed baseline")
       (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
       (git-quiet! repo "update-ref" "refs/remotes/origin/main" "HEAD")
       (make-directory* (build-path root ".planning" "waves"))
       (write-plan! root 0 "Checkpoint" "checkpoint")
       (write-wave-doc! root
                        0
                        "checkpoint"
                        '("q/source.rkt" "q/tests/probe.rkt")
                        "raco test tests/probe.rkt")
       (define record (migrate-campaign! root))
       (check-eq? (campaign-record-execution-policy record) 'owned-red-v1)
       (persist-campaign! root record)
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                      [current-gsd-verification-registry (make-verification-registry)])
         (define outcome
           (run-campaign-wave
            root
            record
            0
            #:fence-token 1
            #:timeout-sec 180
            #:timeout-retries 0
            #:no-change-retries 0
            #:isolate? #t
            #:stall-soft-limit #f
            #:stall-hard-limit #f
            #:runner
            (lambda (_)
              (define phase (current-gsd-author-phase))
              (check-true (hash? phase) "model dispatch must have an enforced phase contract")
              (when (hash? phase)
                (set! phases (append phases (list (hash-ref phase 'phase))))
                (case (hash-ref phase 'phase)
                  [(test-author)
                   (put
                    (build-path (current-directory) "tests" "probe.rkt")
                    (string-append
                     "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
                     "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
                     "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))]
                  [(implementation)
                   (put (build-path (current-directory) "source.rkt")
                        "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")]))
              'ok)
            #:verifier (lambda (_)
                         (check-equal? phases '(test-author implementation))
                         (define check (check-verify-command root 0 #f))
                         (delivery-verification (cadr check) (list check) "actual declared Verify"))))
         (check-equal? phases '(test-author implementation) (format "~s" outcome))
         (define journal (load-delivery-journal root (campaign-plan-id record) 0))
         (check-true (hash? journal) (format "full Verify must retain a real receipt: ~s" outcome))
         (when (hash? journal)
           (define receipt (hash-ref journal 'receipt))
           (check-true (hash? (hash-ref receipt 'owned-proof #f)))
           (define ref (hash-ref receipt 'owned-proof))
           (define store (build-path (current-owned-proof-root) (campaign-plan-id record)))
           (define green
             (load-owned-review-proof store
                                      ref
                                      (hash-ref ref 'binding)
                                      #:command "raco test tests/probe.rkt"
                                      #:normalized-command "raco test tests/probe.rkt"))
           (check-equal? (length (find-owned-red-proofs store green)) 1)
           (define repeated-dispatches 0)
           (parameterize ([current-directory (hash-ref green 'execution-cwd)])
             (run-proactive-author-phases! root
                                           (campaign-plan-id record)
                                           0
                                           (hash-ref receipt 'attempt-id)
                                           (hash-ref receipt 'attempt-fence)
                                           (lambda ()
                                             (set! repeated-dispatches (add1 repeated-dispatches))
                                             'ok)
                                           #:guard (lambda () #t)))
           (check-equal? repeated-dispatches
                         0
                         "same owned attempt cannot redispatch an already launched implementation"))))
     (lambda () (delete-directory/files root)))))
