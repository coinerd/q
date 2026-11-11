#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 480
;; @covers extensions/gsd/author-execution.rkt extensions/gsd/red-checkpoint.rkt
;; #9898 (milestone-#898 W2): an infra/timeout retry whose red-ready checkpoint
;; revalidates against an attempt branch that ALREADY carries the committed
;; implementation must resume to the owned GREEN verification with a done
;; author-phase outcome. It must not re-dispatch the implementation phase
;; into completed work ("phase made no progress") and must not refuse at the
;; at-most-once launch marker ("implementation already launched"). A retry
;; whose frozen non-test targets are NOT all committed still dispatches
;; normally and still fails closed on an empty phase.
(require rackunit
         json
         racket/file
         racket/path
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/author-execution.rkt"
         "../extensions/gsd/red-checkpoint.rkt"
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt")

(define probe-source-text
  (string-append
   "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
   "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
   "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
(define unfixed-source-text "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
(define fixed-source-text "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")

(define (put path text)
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'truncate))

(define (owner-commit! repo message)
  (git-quiet! repo "add" "--" ".")
  (git-quiet! repo "commit" "-qm" message))

;; Frozen probe module authored by the test-author phase (the exact shape the
;; structured RED probe, the owned RED verification and the candidate probe
;; all authenticate for this fixture family).
(define (author-probe-phase! repo)
  (put (build-path repo "tests" "probe.rkt") probe-source-text)
  (owner-commit! repo "GSD test-author attempt-1"))

;; Drives the RED checkpoint up to red-ready exactly as the proactive author
;; phases do: begin-red-test-author! intent, test authoring commit, then the
;; real structured RED probe + owned RED verification. Returns the validated
;; red-ready state.
(define (capture-red-ready! root repo plan-id attempt)
  (define start (begin-red-test-author! root plan-id 0 repo attempt 1 #:guard (lambda () #t)))
  (check-equal? (hash-ref start 'status) "test-author-started")
  (define baseline (hash-ref (hash-ref start 'baseline-source) 'head))
  (author-probe-phase! repo)
  (define checkpoint
    (capture-red-checkpoint! root plan-id 0 repo baseline attempt 1 #:guard (lambda () #t)))
  (check-equal? (hash-ref checkpoint 'status) "red-ready" (format "~s" checkpoint))
  checkpoint)

;; The byte-exact checkout string run-proactive-author-phases! derives from
;; current-directory: a directory path carries the trailing separator, and
;; the checkpoint pins that string (validate-at compares it literally).
(define (executor-repo repo)
  (path->string (simplify-path (string->path (string-append (path->string repo) "/")))))

;; Builds the two-tier campaign fixture (repo q/ + .planning) whose wave
;; freezes q/source.rkt + q/tests/probe.rkt with Verify "raco test
;; tests/probe.rkt", then migrates the campaign to seed the frozen snapshot
;; under the owned-red-v1 policy.
(define (make-campaign-fixture!)
  (define root (make-temporary-file "retry-satisfied-~a" 'directory))
  (define repo (build-path root "q"))
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
  (put (build-path repo "source.rkt") unfixed-source-text)
  (git-quiet! repo "add" ".")
  (git-quiet! repo "commit" "-qm" "unfixed baseline")
  (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
  (git-quiet! repo "update-ref" "refs/remotes/origin/main" "HEAD")
  ;; The coordinator dispatches author phases from a delivery branch, never
  ;; main — committed-delivery-snapshot refuses main/master outright.
  (git-quiet! repo "checkout" "-q" "-b" "campaign/retry-satisfied/w0")
  (make-directory* (build-path root ".planning" "waves"))
  (write-plan! root 0 "Retry satisfied" "retry-satisfied")
  (write-wave-doc! root
                   0
                   "retry-satisfied"
                   '("q/source.rkt" "q/tests/probe.rkt")
                   "raco test tests/probe.rkt")
  (define record (migrate-campaign! root))
  (check-eq? (campaign-record-execution-policy record) 'owned-red-v1)
  (persist-campaign! root record)
  (list root repo (campaign-plan-id record)))

;; The durable checkpoint state under root/red-checkpoints/<plan>/W0/.
(define (load-checkpoint-state root plan-id)
  (define wave-dir (build-path root "red-checkpoints" plan-id "W0"))
  (define checkpoint-dir
    (for/first ([entry (in-list (directory-list wave-dir))]
                #:when (directory-exists? (build-path wave-dir entry)))
      (build-path wave-dir entry)))
  (call-with-input-file (build-path checkpoint-dir "checkpoint.json") read-json))

(define (green-captured? root plan-id)
  (define state (load-checkpoint-state root plan-id))
  (and (hash? (hash-ref state 'green #f))
       (equal? (hash-ref (hash-ref state 'green #f) 'status #f) "captured")))

(module+ test
  ;; The canary terminal for #9898: attempt N committed the deliverable after
  ;; begin-red-implementation! persisted the at-most-once launch marker, then
  ;; the invocation died before the wave completed. The retry (same attempt,
  ;; revalidated red-ready checkpoint) must detect the satisfied phase and
  ;; resume at the owned GREEN verification — never re-dispatch, never
  ;; refuse.
  (test-case "author-execution: retry over a committed implementation resumes GREEN verification past the launch marker"
    (match-define (list root repo plan-id) (make-campaign-fixture!))
    (define exec-repo (executor-repo repo))
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                      [current-gsd-verification-registry (make-verification-registry)])
         (capture-red-ready! root exec-repo plan-id "attempt-1")
         ;; Exact prior-attempt shape: launch marker persisted, then the
         ;; owner committed the implementation, then the invocation died
         ;; before any candidate GREEN capture.
         (begin-red-implementation! root plan-id 0 exec-repo "attempt-1" 1 #:guard (lambda () #t))
         (put (build-path repo "source.rkt") fixed-source-text)
         (owner-commit! repo "GSD implementation attempt-1")
         (check-false (green-captured? root plan-id))
         (define dispatches 0)
         (define outcome
           (parameterize ([current-directory exec-repo])
             (run-proactive-author-phases! root
                                           plan-id
                                           0
                                           "attempt-1"
                                           1
                                           (lambda ()
                                             (set! dispatches (add1 dispatches))
                                             'ok)
                                           #:guard (lambda () #t))))
         (check-equal? (wave-execution-outcome-kind outcome)
                       'done
                       (wave-execution-outcome-message outcome))
         (check-false (regexp-match? #rx"no progress" (wave-execution-outcome-message outcome)))
         (check-equal? dispatches 0 "satisfied phases must not re-dispatch the implementation")
         (check-true (green-captured? root plan-id) "the verify-green path must run")))
     (lambda () (delete-directory/files root))))
  ;; Same satisfied state WITHOUT the launch marker (markerless committed
  ;; attempt branch): the current implementation dispatch returns an empty
  ;; phase and the no-progress guard terminally fails the wave. The
  ;; satisfied detection must rescue this shape too.
  (test-case "author-execution: retry over a committed implementation without a launch marker skips the empty dispatch"
    (match-define (list root repo plan-id) (make-campaign-fixture!))
    (define exec-repo (executor-repo repo))
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                      [current-gsd-verification-registry (make-verification-registry)])
         (capture-red-ready! root exec-repo plan-id "attempt-1")
         (put (build-path repo "source.rkt") fixed-source-text)
         (owner-commit! repo "GSD implementation attempt-1")
         (check-false (green-captured? root plan-id))
         (define dispatches 0)
         (define outcome
           (parameterize ([current-directory exec-repo])
             (run-proactive-author-phases! root
                                           plan-id
                                           0
                                           "attempt-1"
                                           1
                                           (lambda ()
                                             (set! dispatches (add1 dispatches))
                                             'ok)
                                           #:guard (lambda () #t))))
         (check-equal? (wave-execution-outcome-kind outcome)
                       'done
                       (wave-execution-outcome-message outcome))
         (check-false (regexp-match? #rx"no progress" (wave-execution-outcome-message outcome)))
         (check-equal? dispatches 0 "satisfied phases must not re-dispatch the implementation")
         (check-true (green-captured? root plan-id) "the verify-green path must run")))
     (lambda () (delete-directory/files root))))
  ;; Fail-closed contract: a genuinely empty implementation phase (no frozen
  ;; non-test target committed) must still dispatch normally and must still
  ;; fail terminally on the no-progress guard.
  (test-case "author-execution: unsatisfied retry still dispatches and fails closed on an empty phase"
    (match-define (list root repo plan-id) (make-campaign-fixture!))
    (define exec-repo (executor-repo repo))
    (dynamic-wind void
                  (lambda ()
                    (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                                   [current-gsd-verification-registry (make-verification-registry)])
                      (capture-red-ready! root exec-repo plan-id "attempt-1")
                      (define dispatches 0)
                      (define outcome
                        (parameterize ([current-directory exec-repo])
                          (run-proactive-author-phases! root
                                                        plan-id
                                                        0
                                                        "attempt-1"
                                                        1
                                                        (lambda ()
                                                          (set! dispatches (add1 dispatches))
                                                          'ok)
                                                        #:guard (lambda () #t))))
                      (check-equal? (wave-execution-outcome-kind outcome) 'failed)
                      (check-true (regexp-match? #rx"phase made no progress"
                                                 (wave-execution-outcome-message outcome))
                                  (wave-execution-outcome-message outcome))
                      (check-equal? dispatches 1 "unsatisfied phases must dispatch normally")
                      (check-false (green-captured? root plan-id) "no GREEN may be fabricated")))
                  (lambda () (delete-directory/files root)))))
