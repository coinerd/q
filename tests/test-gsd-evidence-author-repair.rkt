#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 600
(require rackunit
         racket/file
         racket/path
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/author-scope.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/evidence-author-journal.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd-planning/execution-policy.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave)
         "../extensions/gsd/evidence-author-repair.rkt")

;; Real owned Verify receipts, real author transitions, and an intentionally
;; unsupported integration harness. Never read or mutate a live campaign.
(define (with-author-fixture proc)
  (define root (make-temporary-file "author-report-~a" 'directory))
  (define repo (build-path root "q"))
  (define (put path text)
    (make-parent-directory* path)
    (display-to-file text path #:exists 'truncate))
  (dynamic-wind
   void
   (lambda ()
     (make-directory repo)
     (git-quiet! repo "init" "-q")
     (hermetic-identity! repo)
     (define remote (build-path root "origin.git"))
     (git-quiet! repo "init" "--bare" "-q" (path->string remote))
     (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
     (git-quiet! repo
                 "config"
                 (string-append "url.file://" (path->string remote) ".insteadOf")
                 "https://github.com/example/q.git")
     (put (build-path repo ".gitignore") "compiled/\n")
     (put (build-path repo "source.rkt") "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
     (git-quiet! repo "add" ".gitignore" "source.rkt")
     (git-quiet! repo "commit" "-qm" "baseline")
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
     (set-campaign-record-execution-policy! record #f)
     (persist-campaign! root record)
     (define plan (campaign-plan-id record))
     (define registry (make-verification-registry))
     (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                    [current-gsd-verification-registry registry])
       (define cwd #f)
       (define (run rec fence runner)
         (run-campaign-wave root
                            rec
                            0
                            #:fence-token fence
                            #:timeout-sec 180
                            #:timeout-retries 0
                            #:no-change-retries 0
                            #:isolate? #t
                            #:stall-soft-limit #f
                            #:stall-hard-limit #f
                            #:runner runner
                            #:verifier (lambda (_)
                                         (define check (check-verify-command root 0 #f))
                                         (delivery-verification (cadr check)
                                                                (list check)
                                                                "actual full frozen Verify"))))
       (define result
         (run record
              1
              (lambda (_)
                (set! cwd (current-directory))
                (put (build-path cwd "source.rkt")
                     "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
                (put (build-path cwd "tests/probe.rkt")
                     (string-append "#lang racket/base\n(require rackunit \"../source.rkt\")\n"
                                    "(module+ test (check-equal? (answer) 1))\n"))
                'ok)))
       (unless (load-delivery-journal root plan 0)
         (error 'fixture "initial delivery failed: ~s" result))
       (dynamic-wind void
                     (lambda () (proc root plan cwd run))
                     (lambda () (verification-close-registry! registry)))))
   (lambda () (delete-directory/files root))))

(define (check-authenticated-unsupported root plan)
  (define j (load-delivery-journal root plan 0))
  (define receipt (hash-ref j 'receipt))
  (check-not-false (load-receipt-owned-proof receipt root plan 0))
  (define context (current-evidence-context root plan 0))
  (check-equal? (hash-ref context 'head) (hash-ref receipt 'head))
  (check-equal? (hash-ref context 'attempt-id) (hash-ref receipt 'attempt-id))
  (define readiness (evidence-readiness root plan 0))
  (check-equal? (hash-ref readiness 'status) "repair-required")
  (check-equal? (hash-ref readiness 'code) "unsupported-harness")
  (check-eq? (hash-ref readiness 'spec) 'null))

(define (capture-marker-report root plan state exhausted?)
  (check-authenticated-unsupported root plan)
  (define path (delivery-journal-path root plan 0))
  (define before (file->bytes path))
  (define marker (journal-evidence-author-repair (load-delivery-journal root plan 0)))
  (check-equal? (hash-ref marker 'state) state)
  (define results
    (for/list ([admit (in-list (list evidence-author-repair-preflight
                                     authorize-evidence-author-repair!))])
      (define result (admit root plan 0 #:guard (lambda () #t)))
      (check-equal? (file->bytes path) before "reporting must be read-only")
      (printf "marker ~a / ~a: ~a (~a)\n"
              state
              (object-name admit)
              (hash-ref result 'code)
              (hash-ref result 'status))
      result))
  (lambda ()
    (for ([result (in-list results)])
      (check-equal?
       (hash-ref result 'code)
       (if exhausted? "evidence-author-allowance-exhausted" "evidence-author-recovery-required"))
      (check-equal? (hash-ref result 'status) (if exhausted? "blocked" "paused"))
      (check-false (hash-ref result 'author-work-started))
      (check-false (hash-ref result 'new-reservation-allowed #t))
      (check-equal? (hash-ref result 'marker-state #f) state)
      (check-equal? (hash-ref result 'marker-context #f) (hash-ref marker 'context))
      (for ([key '(disposition successor-attempt
                               successor-fence
                               successor-receipt-sha256
                               retained-tree-sha256)]
            #:when (hash-has-key? marker key))
        (check-equal? (hash-ref result key #f) (hash-ref marker key)))
      (cond
        [exhausted?
         (check-equal? (hash-ref result 'next-action #f) "explicit-governed-supplemental-replan")
         (check-regexp-match #rx"permanent one-per-wave" (hash-ref result 'message))
         (check-regexp-match #rx"explicit.*governed supplemental" (hash-ref result 'message))
         (check-regexp-match #rx"blocked" (hash-ref result 'message))]
        [else
         (check-equal? (hash-ref result 'next-action #f) "existing-owner-recovery")
         (check-regexp-match #rx"owner recovery" (hash-ref result 'message))]))))

(define (check-marker-report root plan state exhausted?)
  ((capture-marker-report root plan state exhausted?))
  ;; A stale caller cannot obtain even a recovery/advisory admission.
  (define before (file->bytes (delivery-journal-path root plan 0)))
  (check-equal? (hash-ref (evidence-author-repair-preflight root plan 0 #:guard (lambda () #f)) 'code)
                "stale-live-guard")
  (check-equal? (file->bytes (delivery-journal-path root plan 0)) before))
(module+ test
  (test-case "phase validation preserves frozen tests and exact scope"
    (define files '("src/main.rkt" "tests/regression.rkt"))
    (define tests '("tests/regression.rkt"))
    (check-true (validate-author-phase-paths! 'test-author files tests tests))
    (check-true (validate-author-phase-paths! 'implementation files tests '("src/main.rkt")))
    (for ([case (in-list (list (list 'test-author '("src/main.rkt"))
                               (list 'implementation tests)
                               (list 'implementation '("outside.rkt"))
                               (list 'unknown '())
                               (list 'test-author '("tests/../src/main.rkt"))))])
      (check-exn exn:fail?
                 (lambda () (validate-author-phase-paths! (car case) files tests (cadr case)))))
    (check-exn exn:fail?
               (lambda () (validate-author-phase-paths! 'test-author files '("outside.rkt") '()))))
  (test-case "author scope is exact frozen FILES, not a directory or pattern grant"
    (check-true (literal-author-scope? '("src/main.rkt" "tests/regression.rkt")))
    (for ([files (in-list '(() ("src/")
                               ("../outside.rkt")
                               ("/tmp/outside.rkt")
                               ("src/*.rkt")
                               ("src/./main.rkt")
                               ("src//main.rkt")
                               ("src/../main.rkt")
                               ("src\\main.rkt")
                               ("src/main.rkt" "src/main.rkt")))])
      (check-false (literal-author-scope? files))))
  (test-case "completed allowance reports a typed stop, pending/running retain owner recovery"
    (with-author-fixture
     (lambda (root plan cwd run)
       (check-authenticated-unsupported root plan)
       (check-false (journal-evidence-author-repair (load-delivery-journal root plan 0)))
       (define before (file->bytes (delivery-journal-path root plan 0)))
       (check-equal? (hash-ref (evidence-author-repair-preflight root plan 0 #:guard (lambda () #t))
                               'code)
                     "evidence-author-repair-required")
       (check-equal? (file->bytes (delivery-journal-path root plan 0)) before)
       (check-equal? (hash-ref (authorize-evidence-author-repair! root plan 0 #:guard (lambda () #t))
                               'code)
                     "evidence-author-repair-authorized")
       (define check-pending (capture-marker-report root plan "pending" #f))
       (define check-running #f)
       ;; The existing owner, not authorize/preflight, resumes the reservation.
       (define pending (load-campaign-record root plan))
       (set-campaign-wave-status! (car (campaign-record-waves pending)) 'pending)
       (persist-campaign! root pending)
       (define phases '())
       (run pending
            2
            (lambda (_)
              (define phase (hash-ref (current-gsd-author-phase) 'phase))
              (set! phases (append phases (list phase)))
              (when (eq? phase 'test-author)
                (set! check-running (capture-marker-report root plan "running" #f))
                (display-to-file ";; retained unsupported integration harness\n"
                                 (build-path cwd "tests/probe.rkt")
                                 #:exists 'append))
              'ok))
       (check-equal? phases '(test-author implementation))
       (check-equal? (length (hash-ref (load-delivery-journal root plan 0) 'receipt-history)) 1)
       (check-marker-report root plan "completed" #t)
       (check-pending)
       (check-running))))
  (for ([disposition '(no-edit session-lost)])
    (test-case (format "retained ~a allowance is permanent and cannot reserve again" disposition)
      (with-author-fixture
       (lambda (root plan cwd run)
         (define context (current-evidence-context root plan 0))
         (reserve-evidence-author-repair! root
                                          plan
                                          0
                                          context
                                          (frozen-author-files root plan 0)
                                          #:guard (lambda () #t))
         (start-evidence-author-repair! root plan 0 "existing-successor" 2 #:guard (lambda () #t))
         ((if (eq? disposition 'no-edit)
              complete-evidence-author-no-edit!
              abandon-evidence-author-session!)
          root
          plan
          0
          (hash-ref context 'tree)
          #:guard (lambda () #t))
         (check-marker-report root plan "retained" #t)))))
  (test-case "an absent live guard grants no author permission"
    (check-equal? (hash-ref (evidence-author-repair-preflight "." "unused" 0 #:guard (lambda () #f))
                            'code)
                  "stale-live-guard")))
