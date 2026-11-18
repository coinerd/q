#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @timeout 1500
;; @covers extensions/gsd/delivery-coordinator.rkt
;; Real default coordinator + real owned-Verify world + local Git remote. Only
;; the independent model response is deterministic; no controller status is
;; faked (the world is built by tests/helpers/owned-audit-fixture.rkt).
;; 1500 matches the sibling auto-review-loop fixture: both run full real
;; owned-Verify generations, which a loaded 4-CPU runner stretches past the
;; previous 900 s fixture budget (production deadlines are unchanged).
(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         json
         "../sandbox/subprocess.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         (only-in "helpers/auto-review-owned-fixture.rkt" precompile-delivery-controller-scripts!))
(define-runtime-path owned-audit-fixture "helpers/owned-audit-fixture.rkt")
(module+ test
  (test-case "default production coordinator invokes installed independent runner at exact head/generation and replays remote rejection"
    ;; Fixture setup precompile: the controller's racket subprocesses are
    ;; capped at 60 s per command, so their closure must already be zo-warm
    ;; before the coordinator runs (see helpers/auto-review-owned-fixture.rkt).
    (precompile-delivery-controller-scripts!)
    ;; QAR_GENERATIONS=0 / =1 (or `0,1`) selects a subset of the two real owned
    ;; Verify generations, the same discipline as QAR_ONLY in the sibling
    ;; auto-review suites: each generation is an independent world + coordinator
    ;; round trip, so a loaded host can be bisected case by case. The default is
    ;; both generations, which is what CI runs.
    (define generations
      (let ([raw (getenv "QAR_GENERATIONS")])
        (if raw
            (for/list ([part (in-list (string-split raw ","))])
              (string->number (string-trim part)))
            '(0 1))))
    (for ([generation (in-list generations)])
      (define temp (make-temporary-file "production-review-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define manifest (make-campaign-manifest 1 "w0" '() '() (path->string temp)))
         (define plan (campaign-manifest-hash manifest))
         ;; Real declared owned-Verify world, built in a CHILD process by the
         ;; native helper (the same shape as the module this replaces: the
         ;; caller's test process stays small so its later forks stay cheap).
         (define setup
           (run-subprocess (path->string (find-executable-path "racket"))
                           #:args (list (path->string owned-audit-fixture)
                                        "production-world"
                                        (path->string temp)
                                        plan
                                        "0"
                                        (number->string generation))
                           #:timeout 600))
         (check-equal? (subprocess-result-exit-code setup)
                       0
                       (subprocess-result-stderr setup))
         (define world (string->jsexpr (subprocess-result-stdout setup)))
         (define readiness (hash-ref world 'readiness))
         (check-not-false (member (hash-ref readiness 'code)
                                  '("missing-meaningful-red-proof"
                                    "legacy-red-ready"
                                    "registered-probe-ready"))
                          (format "underlying fixture readiness: ~s" readiness))
         (define root (string->path (hash-ref world 'root)))
         (check-true (directory-exists? (build-path root "q")))
         (define receipt (hash-ref world 'receipt))
         (define rec
           (make-campaign-record
            plan
            manifest
            (list (make-campaign-wave 0 "w0" 'awaiting-delivery 1 (campaign-attempt "attempt-5" 2 0)))
            #f
            2
            'test
            0
            0))
         (persist-campaign! root rec)
         (define calls 0)
         (define findings '("  preserve exact whitespace  " "a=b; findings: exact\nsecond line"))
         (define (runner request)
           (set! calls (add1 calls))
           (check-equal? (hash-ref request 'head) (hash-ref receipt 'head))
           (check-equal? (hash-ref request 'branch) (hash-ref receipt 'branch))
           (check-equal? (hash-ref request 'attempt-id) "attempt-5")
           (check-equal? (hash-ref request 'attempt-fence) 2)
           (check-equal? (hash-ref request 'generation) generation)
           (check-true (hash-ref request 'fresh-session-required))
           (check-true (hash-ref request 'non-author-review-required))
           (define staged (build-path (hash-ref request 'staging) (hash-ref request 'review)))
           (check-true (file-exists? staged))
           (check-regexp-match (regexp (hash-ref receipt 'head)) (file->string staged))
           (define authority (hash-ref request 'owned-authority))
           (define bundle (build-path (hash-ref request 'staging) (hash-ref authority 'bundle)))
           (define proof-file
             (build-path bundle (string-append (hash-ref request 'proof-sha256) ".json")))
           (define proof (call-with-input-file proof-file read-json))
           (check-equal? (hash-ref (hash-ref proof 'binding) 'head) (hash-ref receipt 'head))
           (check-true (positive? (string-length
                                   (file->string
                                    (build-path bundle (hash-ref (hash-ref proof 'log) 'path))))))
           (check-true (string-contains? (file->string (build-path (hash-ref world 'repo)
                                                                   "src/gsd-fixture-widget.rkt"))
                                         "#lang"))
           (define red-sha (hash-ref (hash-ref (hash-ref authority 'red) 'reference) 'sha256))
           (for ([path (in-directory bundle)]
                 #:when (file-exists? path))
             (file->bytes path))
           (hasheq 'kind
                   "request-changes"
                   'reviewer
                   "independent-production-test-session"
                   'scope
                   (make-string 70 #\s)
                   'report
                   (make-string 70 #\r)
                   'findings
                   findings
                   'audit-ack
                   (hasheq 'mode
                           "owned-proof-audit-v1"
                           'verification-origin
                           "audited-owned"
                           'source-read
                           #t
                           'raw-output-read
                           #t
                           'binding
                           (hash-ref authority 'audit-binding)
                           'red-ack
                           (hasheq 'proof-sha256 red-sha 'raw-output-read #t))))
         ;; Simulate publication -> checkpoint crash: do not consume this outcome.
         (define published
           (parameterize ([current-gsd-auto-review-runner runner])
             (default-delivery-coordinator root plan 0)))
         (check-equal? (delivery-outcome-kind published)
                       'review-rejected
                       (delivery-outcome-message published))
         (check-equal? calls 1)
         (define before (load-delivery-journal root plan 0))
         (check-false (hash-ref before 'review-rejection #f))
         (define history (hash-ref before 'receipt-history '()))
         (delete-directory/files (review-dispatch-staging-path root plan 0 generation))
         (define (resume)
           (parameterize ([current-gsd-auto-review-runner
                           (lambda (_) (error 'test "recovery must not invoke reviewer"))])
             (run-coordinator-checkpoint root
                                         plan
                                         (car (campaign-record-waves rec))
                                         "pending"
                                         default-delivery-coordinator
                                         #:on-advance (lambda () 'retry)
                                         #:on-blocked (lambda (_) 'blocked))))
         (check-equal? (resume) 'retry)
         (check-equal? (resume) 'retry)
         (define after (load-delivery-journal root plan 0))
         (check-equal? (hash-ref after 'review-rejection-count) 1)
         (check-equal? (hash-ref (hash-ref after 'review-rejection) 'findings) findings)
         (check-equal? (hash-ref after 'receipt-history '()) history)
         (check-equal?
          (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan))))
          'failed))
       (lambda () (delete-directory/files temp))))))
