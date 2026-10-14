#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/review-dispatch.rkt
;; Automatic independent review dispatch (BUG_PLAN automatic-review slice B).
;;
;; The dispatch port drives a FRESH non-author reviewer session through the
;; exported current-gsd-auto-review-runner hook whenever the trusted
;; controller reports a genuinely MISSING implementation trio or a staged
;; pending binding review. Laws under test:
;;   * no runner installed (default #f) preserves the manual fixture path —
;;     the dispatcher declines (#f) and the caller falls back to the
;;     unchanged validation-only controller action;
;;   * the runner port accepts BOTH shapes: a typed request→result callback
;;     (arity 1) and a factory producing a fresh prompt→result session
;;     (arity 0); the typed shape wins when both arities are accepted;
;;   * the typed statuses stay separate: awaiting-review (missing info),
;;     review-rejected (explicit head-bound REQUEST_CHANGES carrying the
;;     exact receipt head/branch/attempt/fence + findings), review-infra
;;     (transport/timeout/stale/malformed reviewer output — NEVER a
;;     rejection and NEVER an approval);
;;   * the reviewer session never receives credentials (the runner is a
;;     plain procedure invoked with the typed request only; the controller
;;     subprocess boundary stays the only credential path);
;;   * the receipt pin (head/branch/attempt-id/fence) captured before the
;;     dispatch must still hold after the reviewer ran — drift is a typed
;;     review-infra stale stop, never a rejection;
;;   * the coordinator surfaces 'review-rejected / 'review-infra outcomes
;;     without touching the journal stage.
(require rackunit
         racket/file
         racket/string
         json
         racket/format
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  load-delivery-journal
                  record-delivery-receipt!
                  reconcile-repair-tail-receipt!
                  update-delivery-journal!)
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/review-dispatch.rkt")

;; ---------------------------------------------------------------------------
;; Fixtures (same shape as tests/test-gsd-delivery-coordinator.rkt)
;; ---------------------------------------------------------------------------

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
  (define dir (make-temporary-file "review-dispatch-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display "# Plan: Review dispatch test\n\n## Waves\n\n" out)
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

;; A scripted trusted-controller seam: dispatches on the --phase argument and
;; returns parsed-status hashes; records every argv it saw.
(define (scripted-python responses)
  (define seen '())
  (define (run args)
    (set! seen (cons args seen))
    (define phase
      (cond
        [(memq '--phase (map (lambda (s) (string->symbol s)) args))
         =>
         cadr]
        [else (error 'scripted-python "no --phase in ~a" args)]))
    (cond
      [(assq phase responses)
       =>
       (lambda (pair) ((cdr pair)))]
      [else (error 'scripted-python "unexpected phase ~a" phase)]))
  (values run (lambda () (reverse seen))))

(define (prepare-awaiting-request staging)
  (hasheq 'status
          "awaiting-review"
          'stage
          "implementation-review"
          'request
          (hasheq 'kind
                  "implementation-review"
                  'plan
                  "p"
                  'wave
                  0
                  'generation
                  1
                  'head
                  (make-string 40 #\a)
                  'branch
                  "campaign/test"
                  'attempt-id
                  "attempt-1"
                  'attempt-fence
                  7
                  'evidence
                  "docs/reports/gsd-wave-evidence/p-w0-r1.rktd"
                  'review
                  "docs/reports/gsd-wave-reviews/p-w0-r1.rktd"
                  'validation
                  "docs/reports/gsd-wave-validation/p-w0-r1.rktd"
                  'content-digest
                  (make-string 64 #\d)
                  'staging
                  staging
                  'prompt
                  "Review the wave implementation at the pinned head; the staged trio is gate-red.")))

(define approved-result
  (hasheq 'kind
          "approved"
          'reviewer
          "fresh-independent-reviewer-session"
          'scope
          "independent review of the implementation, gates and provenance at the pinned head"
          'report
          "ran the focused and fast gates; no blocking defect found at the reviewed head"
          'validation
          (hasheq)))

(define rejected-result
  (hasheq 'kind
          "request-changes"
          'reviewer
          "fresh-independent-reviewer-session"
          'scope
          "independent review of the implementation, gates and provenance at the pinned head"
          'report
          "the implementation violates the delivery contract at the reviewed head"
          'findings
          (list "focused tests fail at the pinned head" "format drift in the touched module")))

(module+ test
  (test-case "the runner hook defaults to absent and preserves the manual path"
    (check-false (current-gsd-auto-review-runner))
    (check-false (review-runner-installed?)))

  (test-case "auto-review-dispatch! declines without a runner"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (done-record dir)
                          (define-values (run seen) (scripted-python '()))
                          (check-false (auto-review-dispatch! dir
                                                              (campaign-plan-id rec)
                                                              0
                                                              "implementation-review"
                                                              #:generation 1
                                                              #:run-python run))
                          (check-equal? (seen) '()))))

  (test-case "an approved fresh reviewer flows through prepare → runner → finalize"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define requests '())
       (define-values (run seen)
         (scripted-python (list (cons 'prepare (lambda () (prepare-awaiting-request staging)))
                                (cons 'finalize
                                      (lambda ()
                                        (hasheq 'status
                                                "reviewed"
                                                'head
                                                (make-string 40 #\a)
                                                'reviewed-sha
                                                (make-string 40 #\a)))))))
       (define runner
         (lambda (request)
           (set! requests (cons request requests))
           approved-result))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner runner
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       ;; the runner saw the typed request carrying the pinned receipt facts
       (define request (car requests))
       (check-equal? (hash-ref request 'head) (make-string 40 #\a))
       (check-equal? (hash-ref request 'branch) "campaign/test")
       (check-equal? (hash-ref request 'attempt-id) "attempt-1")
       (check-equal? (hash-ref request 'attempt-fence) 7)
       (check-equal? (hash-ref request 'kind) "implementation-review")
       (check-equal?
        (hash-ref request 'prompt)
        "Review the wave implementation at the pinned head; the staged trio is gate-red.")
       ;; the reviewer's typed result was written into the staging area for
       ;; the trusted controller (never passed as prose through argv)
       (define written (call-with-input-file (build-path staging "reviewer-output.json") read-json))
       (check-equal? (hash-ref written 'kind) "approved")
       (check-equal? (hash-ref written 'reviewer) "fresh-independent-reviewer-session")
       ;; both controller phases ran with the pinned identity
       (check = (length (seen)) 2))))

  (test-case "a typed REQUEST_CHANGES effect carries head/branch/attempt/fence/findings"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define-values (run _seen)
         (scripted-python (list (cons 'prepare (lambda () (prepare-awaiting-request staging)))
                                (cons 'finalize
                                      (lambda ()
                                        (hasheq 'status
                                                "review-rejected"
                                                'stage
                                                "implementation-review"
                                                'head
                                                (make-string 40 #\a)
                                                'branch
                                                "campaign/test"
                                                'attempt-id
                                                "attempt-1"
                                                'attempt-fence
                                                7
                                                'postmerge
                                                #f
                                                'findings
                                                (list "focused tests fail at the pinned head")))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_) rejected-result)
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'review-rejected)
       (define data (delivery-effect-result-data result))
       (check-equal? (hash-ref data 'head) (make-string 40 #\a))
       (check-equal? (hash-ref data 'branch) "campaign/test")
       (check-equal? (hash-ref data 'attempt-id) "attempt-1")
       (check-equal? (hash-ref data 'attempt-fence) 7)
       (check-equal? (hash-ref data 'findings) (list "focused tests fail at the pinned head"))
       (check-equal? (hash-ref data 'postmerge) #f))))

  (test-case "the factory shape (arity 0) also produces a fresh prompt->result reviewer"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define prompts '())
       (define-values (run _seen)
         (scripted-python
          (list (cons 'prepare (lambda () (prepare-awaiting-request staging)))
                (cons 'finalize (lambda () (hasheq 'status "reviewed" 'head (make-string 40 #\a)))))))
       (define factory
         (lambda ()
           (lambda (prompt)
             (set! prompts (cons prompt prompts))
             approved-result)))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner factory
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       (check-equal? (length prompts) 1)
       (check-true (string-contains? (car prompts) "gate-red")))))

  (test-case "malformed reviewer output is review-infra, never a rejection or approval"
    (for ([bad
           (list
            (hasheq 'kind "approved")
            (hasheq 'kind "bogus" 'reviewer "x")
            (hasheq 'kind "request-changes" 'reviewer "x" 'findings '())
            (hasheq 'kind "request-changes" 'reviewer "x" 'findings (list "") 'scope "s" 'report "r")
            (hasheq 'kind "approved" 'reviewer "PENDING" 'scope "s" 'report "r")
            ;; A review-failed reason that merely copies schema placeholder
            ;; text is malformed output, not a factual failure account.
            (hasheq 'kind "review-failed" 'reason "infrastructure only")
            (hasheq 'kind "review-failed" 'reason "Infrastructure failure only.")
            (hasheq 'kind "review-failed" 'reason "factual failure detail")
            (hasheq 'kind "review-failed" 'reason "placeholder")
            (hasheq 'kind "review-failed" 'reason "N/A")
            "not even a hash"
            #f)])
      (check-eq? (runner-result-kind bad) 'invalid))
    (check-eq? (runner-result-kind approved-result) 'approved)
    (check-eq? (runner-result-kind rejected-result) 'request-changes)
    (check-eq? (runner-result-kind (hasheq 'kind "review-failed" 'reason "session crashed"))
               'review-failed)
    ;; Genuine factual reasons that merely mention infrastructure pass.
    (check-eq? (runner-result-kind
                (hasheq 'kind
                        "review-failed"
                        'reason
                        "review session timeout or infrastructure failure; no rejection retry"))
               'review-failed)
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define-values (run _seen)
         (scripted-python (list (cons 'prepare (lambda () (prepare-awaiting-request staging))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_) (hasheq 'kind "approved"))
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'review-infra)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "malformed")))))

  (test-case "a review-failed runner session is review-infra and never reaches finalize"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define-values (run seen)
         (scripted-python (list (cons 'prepare (lambda () (prepare-awaiting-request staging))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_)
                                           (hasheq 'kind "review-failed" 'reason "session timed out"))
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'review-infra)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "session timed out"))
       ;; only the prepare phase ran; the trusted controller never saw a
       ;; finalize call for a failed reviewer session
       (check = (length (seen)) 1))))

  (test-case "controller review-infra statuses pass through typed, not as blocked prose"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define-values (run _seen)
         (scripted-python
          (list (cons 'prepare (lambda () (prepare-awaiting-request staging)))
                (cons 'finalize
                      (lambda ()
                        (hasheq 'status
                                "review-infra"
                                'reason
                                "non-fast-forward: the remote ref diverged during review"))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_) approved-result)
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'review-infra)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "non-fast-forward")))))

  (test-case "receipt drift during the dispatch is a stale review-infra stop"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define staging (review-dispatch-staging-path dir plan 0 1))
       (define-values (run _seen)
         (scripted-python (list (cons 'prepare (lambda () (prepare-awaiting-request staging)))
                                (cons 'finalize
                                      (lambda ()
                                        ;; the receipt moved while the reviewer ran: the
                                        ;; dispatch must refuse its own result
                                        (reconcile-repair-tail-receipt!
                                         dir
                                         plan
                                         0
                                         (hash-set (receipt-head) 'head (make-string 40 #\e))
                                         #:expected-attempt-id "attempt-1"
                                         #:expected-fence 7
                                         #:head-ancestor? (lambda (_o n) #t))
                                        (hasheq 'status "reviewed" 'head (make-string 40 #\a)))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_) approved-result)
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'review-infra)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "stale")))))

  (test-case "an already-reviewed controller state needs no reviewer dispatch"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define plan (campaign-plan-id (done-record dir)))
                          (define-values (run _seen)
                            (scripted-python (list (cons 'prepare
                                                         (lambda ()
                                                           (hasheq 'status
                                                                   "reviewed"
                                                                   'head
                                                                   (make-string 40 #\a)
                                                                   'reviewed-sha
                                                                   (make-string 40 #\a)))))))
                          (define calls 0)
                          (define result
                            (auto-review-dispatch! dir
                                                   plan
                                                   0
                                                   "implementation-review"
                                                   #:generation 1
                                                   #:runner (lambda (_)
                                                              (set! calls (add1 calls))
                                                              approved-result)
                                                   #:run-python run))
                          (check-eq? (delivery-effect-result-kind result) 'ok)
                          (check-equal? calls 0))))

  (test-case "a controller refusal (real drift) stays blocked, never dispatches"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (define-values (run _seen)
         (scripted-python
          (list (cons 'prepare
                      (lambda ()
                        (hasheq 'status
                                "delivery-pending"
                                'reason
                                (string-append
                                 "receipt head aaa… is verified but later wave-branch commits touch "
                                 "non-evidence paths: src/file.txt")))))))
       (define result
         (auto-review-dispatch! dir
                                plan
                                0
                                "implementation-review"
                                #:generation 1
                                #:runner (lambda (_) approved-result)
                                #:run-python run))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "non-evidence paths")))))

  (test-case "staging paths are generation-scoped and preserve old generations"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define plan (campaign-plan-id (done-record dir)))
       (check-equal? (review-dispatch-staging-path dir plan 0 0)
                     (path->string (build-path dir ".planning" "campaigns" plan "review-w0-g0")))
       (check-equal? (review-dispatch-staging-path dir plan 0 1)
                     (path->string (build-path dir ".planning" "campaigns" plan "review-w0-g1")))
       ;; binding staging keeps its deterministic generation-scoped family
       (check-equal? (default-binding-staging-path dir plan 0)
                     (path->string (build-path dir ".planning" "campaigns" plan "binding-w0"))))))

  (test-case "the coordinator surfaces review-rejected without touching the journal"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (update-delivery-journal! dir plan 0 (hasheq 'stage "context-ready"))
       (define (controller _b _p _w target)
         (delivery-effect-result 'review-rejected
                                 (hasheq 'stage
                                         target
                                         'head
                                         (make-string 40 #\a)
                                         'branch
                                         "campaign/test"
                                         'attempt-id
                                         "attempt-1"
                                         'attempt-fence
                                         7
                                         'findings
                                         (list "focused tests fail at the pinned head"))))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'review-rejected)
       (check-true (string-contains? (delivery-outcome-message outcome) "focused tests fail"))
       (check-true (string-contains? (delivery-outcome-message outcome) (make-string 40 #\a)))
       (check-true (string-contains? (delivery-outcome-message outcome) "campaign/test"))
       (check-equal? (hash-ref (load-delivery-journal dir plan 0) 'stage) "context-ready"))))

  (test-case "the coordinator surfaces review-infra without touching the journal"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (update-delivery-journal! dir plan 0 (hasheq 'stage "context-ready"))
       (define (controller _b _p _w target)
         (delivery-effect-result
          'review-infra
          (hasheq 'stage target 'reason "review infrastructure failure: session timed out")))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'review-infra)
       (check-true (string-contains? (delivery-outcome-message outcome) "session timed out"))
       (check-equal? (hash-ref (load-delivery-journal dir plan 0) 'stage) "context-ready"))))

  (test-case "typed statuses map through the default controller interpreter"
    (check-eq? (delivery-effect-result-kind
                (default-delivery-controller-interpret
                 "implementation-review"
                 (hasheq 'status "review-rejected" 'head (make-string 40 #\a) 'findings (list "f1"))))
               'review-rejected)
    (check-eq? (delivery-effect-result-kind (default-delivery-controller-interpret
                                             "binding-review"
                                             (hasheq 'status "review-infra" 'reason "transport")))
               'review-infra)
    (check-not-false (delivery-effect-result? (delivery-effect-result 'review-rejected)))
    (check-not-false (delivery-effect-result? (delivery-effect-result 'review-infra)))))

(module+ test
  ;; The receipt-bound artifact checkout selector must not falsely reject a
  ;; LAWFUL evidence-only commit made after the receipt (the automatic
  ;; reviewer's trio): the same ancestry + excluded-diff law the trusted
  ;; controller enforces on the branch tip decides, and non-evidence drift
  ;; after the receipt still fails closed.
  (define (scripted-git receipt-head evidence-tip diff-output ancestor?)
    (define listing
      (string-append "worktree /repo\nHEAD 1111111111111111111111111111111111111111\n"
                     "branch refs/heads/main\n"
                     "worktree /repo/wt\nHEAD "
                     evidence-tip
                     "\n"
                     "branch refs/heads/campaign/test\n"))
    (lambda (repo args)
      (cond
        [(equal? args '("worktree" "list" "--porcelain")) (values 0 listing)]
        [(equal? args (list "merge-base" "--is-ancestor" receipt-head evidence-tip))
         (values (if ancestor? 0 1) "")]
        [(equal? args (list "diff" "--name-only" receipt-head evidence-tip)) (values 0 diff-output)]
        [(equal? args '("status" "--porcelain")) (values 0 "")]
        [else (values 1 "")])))

  (test-case "provenance root accepts a clean evidence-only receipt descendant"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (done-record dir)
                          (define plan (campaign-plan-id (migrate-campaign! dir)))
                          (define receipt-head (make-string 40 #\a))
                          (define tip (make-string 40 #\c))
                          (define git
                            (scripted-git receipt-head
                                          tip
                                          (string-append "docs/reports/gsd-wave-evidence/x.rktd\n"
                                                         "docs/reports/gsd-wave-reviews/x.rktd\n"
                                                         "docs/reports/gsd-wave-validation/x.rktd\n")
                                          #t))
                          (check-equal? (default-artifact-provenance-root dir plan 0 #:run-git git)
                                        (string->path "/repo/wt"))
                          ;; the exact receipt head still qualifies (unchanged prior law)
                          (check-equal? (default-artifact-provenance-root
                                         dir
                                         plan
                                         0
                                         #:run-git (scripted-git receipt-head receipt-head "" #f))
                                        (string->path "/repo/wt")))))

  (test-case "provenance root refuses source drift or rewritten history after the receipt"
    (call-with-campaign
     1
     (lambda (dir rec)
       (done-record dir)
       (define plan (campaign-plan-id (migrate-campaign! dir)))
       (define receipt-head (make-string 40 #\a))
       (define tip (make-string 40 #\c))
       ;; non-evidence drift after the receipt: never selected
       (check-false (default-artifact-provenance-root
                     dir
                     plan
                     0
                     #:run-git (scripted-git receipt-head tip "src/file.txt\n" #t)))
       ;; a tip that does not descend from the receipt head: never selected
       (check-false
        (default-artifact-provenance-root
         dir
         plan
         0
         #:run-git (scripted-git receipt-head tip "docs/reports/gsd-wave-evidence/x.rktd\n" #f)))))))

(module+ test
  ;; The PRODUCTION seam against the real controller script: a genuine typed
  ;; refusal (exit 2 delivery-pending JSON) must come back as the parsed
  ;; payload — a blocked stop with the preserved reason, never a raise, never
  ;; an infrastructure failure.
  (test-case "the production seam surfaces typed controller refusals as payloads"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (define seam (default-review-python dir plan 0))
                          (define payload
                            (seam (list "auto-review"
                                        "--phase"
                                        "prepare"
                                        "--stage"
                                        "implementation-review"
                                        "--expected-head"
                                        (make-string 40 #\a)
                                        "--expected-branch"
                                        "campaign/test"
                                        "--campaign-root"
                                        (path->string dir)
                                        "--generation"
                                        "1"
                                        "--staging-root"
                                        (review-dispatch-staging-path dir plan 0 1))))
                          (check-equal? (hash-ref payload 'status #f) "delivery-pending")
                          (check-true (string? (hash-ref payload 'reason #f))))))

  ;; Production routing is proved by test-gsd-auto-review-production.rkt:
  ;; real default coordinator/controller, actual runner invocation, exact
  ;; staged receipt/generation and publication-to-checkpoint recovery. A
  ;; blocked stub repository is NOT evidence that dispatch occurred.
  )
