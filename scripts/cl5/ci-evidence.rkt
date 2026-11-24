#lang racket/base

;; scripts/cl5/ci-evidence.rkt — exact-SHA CI ground truth for the CL5 W2
;; selection experiment (#9982).
;;
;; Approved mechanism (owner decision, CL5 W2): a defect or control state is
;; committed to a uniquely named scratch branch and CI is triggered with
;; `workflow_dispatch` on that branch; the run is then located BY HEAD SHA, so
;; a pull-request synthetic merge commit can never be mistaken for the
;; injected-defect commit.
;;
;; Evidence rules (frozen corpus + review):
;;   * a run that is not `completed` is UNAVAILABLE, never a pass or a failure;
;;   * a cancelled / timed-out / action-required run is UNAVAILABLE;
;;   * a required test producer that was skipped is UNAVAILABLE, because the
;;     comprehensive surface did not actually execute;
;;   * only a completed run with every job concluded success is `pass`, and a
;;     completed run with a failed job is `fail` (with the detecting jobs named).
;;
;; GitHub access goes through scripts/gh_helpers.py (PAT stays in that helper;
;; it is never printed, logged or passed on a command line here).

(require json
         racket/list
         racket/port
         racket/string)

(provide classify-ci-run
         run-for-sha
         required-producers
         expected-optional-jobs
         expected-jobs
         classify-case-evidence
         unavailable-conclusions
         workflow-dispatch!
         find-run-for-sha
         wait-for-run
         jobs-for-run
         ci-verdict-for-sha
         scratch-branch-name
         infrastructure-step?
         failure-kind)

;; Conclusions that mean "the comprehensive surface did not run".
(define unavailable-conclusions '(cancelled timed_out action_required stale))

;; ---- F1: infrastructure failure vs behavioural detection (pre-registered) ----------
;;
;; A job can fail without its tests ever executing: a setup action times out on
;; the network, an installer or a cache restore dies, or the job stops before its
;; lane runs. Counting such a job as a behavioural detection would fabricate the
;; very evidence the W3 protocol needs, and reading it as a "security failure"
;; would misreport a check that never ran.
;;
;; Pre-registered rule (2026-11-24, applies to evidence classified after this
;; revision; earlier cached verdicts are NEVER rewritten):
;;
;;   A failed job is an INFRASTRUCTURE failure when every step that failed is a
;;   setup/action step (the pre-registered name patterns below). Its tests did
;;   not execute, so it is 'unavailable' for that required producer, never
;;   'fail' and never a detector.
;;
;;   A failed job with no step evidence is UNATTRIBUTABLE and is also
;;   'unavailable' (fail closed: never turn missing evidence into a detection).
;;
;;   Any failing step that is not a setup/action step is BEHAVIOURAL: the job's
;;   own command ran and failed, so it remains a named detector.
;;
;; The patterns are deliberately narrow and observable. They describe steps that
;; prepare an environment (GitHub-provided actions and this repository's own
;; `./.github/actions/*` setup actions), not test, lint or aggregation steps.
(define infrastructure-step-patterns
  (list #px"^Set up job$"
        #px"^Run actions/"
        #px"^Run \\./\\.github/actions/"
        #px"^(Cache|Restore|Install|Set up) "))

(define (infrastructure-step? name)
  (for/or ([pattern (in-list infrastructure-step-patterns)])
    (and (regexp-match? pattern name) #t)))

;; 'none / 'behavioural / 'infrastructure / 'unattributable
(define (failure-kind conclusion failed-steps)
  (cond
    [(not (equal? conclusion "failure")) 'none]
    [(null? failed-steps) 'unattributable]
    [(for/and ([step (in-list failed-steps)])
       (infrastructure-step? step))
     'infrastructure]
    [else 'behavioural]))

;; The REQUIRED producers of the authoritative comprehensive surface. Every one
;; of these must be PRESENT in the job list and conclude `success`; absent,
;; duplicated, skipped, cancelled, in-progress or unknown is `unavailable`.
;; (14 jobs, verified against an all-green main run.)
(define required-producers
  '("lint" "lint-quality"
           "security"
           "test-platform"
           "abstraction-audit"
           "release-dry-run"
           "smoke (ubuntu-latest)"
           "test (0)"
           "test (1)"
           "test (2)"
           "workflows (0)"
           "workflows (1)"
           "prepared-env-report"
           "test-aggregate"))

;; Jobs that are part of the graph but legitimately skipped on a
;; workflow_dispatch run of ci.yml: release-readiness is tag-only, fast-env is
;; the self-hosted/push path, gsd-governance is push-only, and
;; workflows-aggregate only runs on push. They must still APPEAR in the job
;; list (a missing job means a truncated/incomplete graph), but a skip here is
;; expected and is not counted as an unavailable producer.
(define expected-optional-jobs
  '("fast-env" "gsd-governance" "release-readiness" "workflows-aggregate"))

(define expected-jobs (append required-producers expected-optional-jobs))

(define (job-name job)
  (hash-ref job 'name ""))

;; classify-ci-run : run-hash (listof job-hash) -> hash
;; 'verdict is 'pass / 'fail / 'unavailable; 'detecting names the failed jobs
;; and their failed steps; 'unavailable-reasons explains 'unavailable.
(define (classify-ci-run run jobs)
  (define status (hash-ref run 'status "unknown"))
  (define conclusion (hash-ref run 'conclusion #f))
  (define job-summaries
    (for/list ([job (in-list jobs)])
      (define failed-steps
        (for/list ([step (in-list (hash-ref job 'steps '()))]
                   #:when (equal? (hash-ref step 'conclusion #f) "failure"))
          (hash-ref step 'name "")))
      (hash 'name
            (job-name job)
            'status
            (hash-ref job 'status #f)
            'conclusion
            (hash-ref job 'conclusion #f)
            'failed-steps
            failed-steps
            'failure-kind
            (failure-kind (hash-ref job 'conclusion #f) failed-steps))))
  (define names (map (lambda (j) (hash-ref j 'name)) job-summaries))
  ;; Duplicate job names mean the payload cannot be read as a complete graph.
  (define duplicates
    (for/list ([name (in-list (remove-duplicates names))]
               #:when (> (count (lambda (n) (equal? n name)) names) 1))
      name))
  ;; Missing expected jobs: the comprehensive surface did not fully execute,
  ;; even if every job that DID report was green.
  (define missing
    (for/list ([name (in-list expected-jobs)]
               #:when (not (member name names)))
      name))
  (define summary-of
    (lambda (name)
      (for/first ([j (in-list job-summaries)]
                  #:when (equal? (hash-ref j 'name) name))
        j)))
  (define (non-success-conclusion? c)
    (and c (memq (string->symbol c) '(skipped cancelled timed_out action_required neutral stale))))
  (define bad-required
    (for/list ([name (in-list required-producers)]
               #:when (summary-of name)
               #:do [(define j (summary-of name))]
               #:when (and (not (equal? (hash-ref j 'conclusion) "success"))
                           (not (equal? (hash-ref j 'conclusion) "failure"))))
      (format "required producer ~a was ~a/~a" name (hash-ref j 'status) (hash-ref j 'conclusion))))
  (define unavailable-reasons
    (append
     (cond
       [(not (equal? status "completed")) (list (format "run status is ~a, not completed" status))]
       [(and conclusion (memq (string->symbol conclusion) unavailable-conclusions))
        (list (format "run conclusion is ~a" conclusion))]
       [else '()])
     (for/list ([name (in-list duplicates)])
       (format "duplicate job name ~a in the job list" name))
     (for/list ([name (in-list missing)])
       (format "expected job ~a is absent from the job list" name))
     bad-required))
  (define failed
    (for/list ([summary (in-list job-summaries)]
               #:when (equal? (hash-ref summary 'conclusion) "failure"))
      summary))
  ;; F1: a required producer whose steps never reached its own command did not
  ;; execute the comprehensive surface. That is unavailable evidence, not a
  ;; detection; the reason names the producer and the failing step(s).
  (define infrastructure-failures
    (for/list ([summary (in-list failed)]
               #:when (memq (hash-ref summary 'failure-kind) '(infrastructure unattributable))
               #:when (member (hash-ref summary 'name) required-producers))
      summary))
  (define infrastructure-reasons
    (for/list ([summary (in-list infrastructure-failures)])
      (format "required producer ~a ~a (failed steps: ~a); its tests did not execute"
              (hash-ref summary 'name)
              (if (eq? (hash-ref summary 'failure-kind) 'infrastructure)
                  "failed in setup/action steps"
                  "failed with no step evidence")
              (string-join (hash-ref summary 'failed-steps) ", "))))
  (define detecting
    (for/list ([summary (in-list failed)]
               #:when (eq? (hash-ref summary 'failure-kind) 'behavioural))
      summary))
  (define all-unavailable-reasons (append unavailable-reasons infrastructure-reasons))
  (hash 'verdict
        (cond
          [(pair? all-unavailable-reasons) 'unavailable]
          [(pair? failed) 'fail]
          [(equal? conclusion "success") 'pass]
          [else 'unavailable])
        'run-status
        status
        'run-conclusion
        conclusion
        'head-sha
        (hash-ref run 'head_sha #f)
        'run-id
        (hash-ref run 'id #f)
        'jobs
        job-summaries
        'missing-jobs
        missing
        'duplicate-jobs
        duplicates
        'detecting
        detecting
        'infrastructure-failures
        infrastructure-failures
        'unavailable-reasons
        all-unavailable-reasons))

;; Full evidence for one case state: the shadow-selected run AND the CI
;; comprehensive verdict, plus the clean controls of both surfaces.
;;
;;   'detected   selected fails on the defect, comprehensive fails, and BOTH
;;               clean controls pass
;;   'missed     selected passes on the defect while comprehensive fails
;;   'unavailable  anything incomplete: a control that did not pass, a timeout,
;;               an unavailable CI run, or the two surfaces disagreeing in the
;;               other direction (comprehensive green but selected red means the
;;               injection did not reproduce the documented defect)
;;   'not-detected neither surface fails — the injection is not a valid case
(define (classify-case-evidence selected-defect
                                selected-control
                                comprehensive-defect
                                comprehensive-control)
  (define (unavailable? v)
    (eq? v 'unavailable))
  (cond
    [(or (unavailable? selected-defect)
         (unavailable? selected-control)
         (unavailable? comprehensive-defect)
         (unavailable? comprehensive-control))
     'unavailable]
    [(or (eq? selected-control 'fail) (eq? comprehensive-control 'fail)) 'unavailable]
    [(and (eq? selected-defect 'fail) (eq? comprehensive-defect 'fail)) 'detected]
    [(and (eq? selected-defect 'pass) (eq? comprehensive-defect 'fail)) 'missed]
    ;; The surfaces disagree the other way: the selector rejects a tree the
    ;; comprehensive surface accepts, so the injection did not reproduce the
    ;; documented defect. That is not a selection signal.
    [(and (eq? selected-defect 'fail) (eq? comprehensive-defect 'pass)) 'unavailable]
    [else 'not-detected]))

;; Pure filter: the CI run whose head SHA is exactly `sha`.
(define (run-for-sha runs sha)
  (for/first ([run (in-list runs)]
              #:when (and (equal? (hash-ref run 'head_sha #f) sha)
                          (equal? (hash-ref run 'name #f) "CI")))
    run))

;; A scratch branch name that identifies the experiment, case and state.
(define (scratch-branch-name case-id label nonce)
  (format "cl5-w2/~a-~a-~a"
          (string-downcase (symbol->string case-id))
          (string-downcase (symbol->string label))
          nonce))

;; ---- effects (through scripts/gh_helpers.py; no token ever printed) --------------

;; Capture a short synchronous subprocess: both streams are drained while the
;; child runs, then the process is waited for.
(define (capture cwd program args)
  (define-values (proc out in err)
    (parameterize ([current-directory cwd])
      (apply subprocess #f #f #f program args)))
  (close-output-port in)
  (define err-box (box ""))
  (define err-thread (thread (lambda () (set-box! err-box (port->string err)))))
  (define stdout (port->string out))
  (thread-wait err-thread)
  (define stderr (unbox err-box))
  (subprocess-wait proc)
  (values (subprocess-status proc) stdout (or stderr "")))

;; Run one gh_helpers REST call in `repo-root` and decode its JSON body.
(define (gh-json repo-root method path [data #f])
  (define code
    (string-append "import sys,json;sys.path.insert(0,'scripts');import gh_helpers as h;"
                   "h.init();m,p=sys.argv[1],sys.argv[2];"
                   "d=json.loads(sys.argv[3]) if len(sys.argv)>3 and sys.argv[3] else None;"
                   "r=h.rest_api(m,p,d);print(json.dumps(r) if r is not None else 'null')"))
  (define python (or (find-executable-path "python3") "python3"))
  ;; gh_helpers lives in the WORKSPACE scripts/ dir (the q checkout has no such
  ;; module); the caller passes the workspace root as `repo-root`.
  (define-values (status stdout stderr)
    (capture repo-root
             python
             (list "-c"
                   code
                   method
                   path
                   (if data
                       (jsexpr->string data)
                       ""))))
  (unless (equal? status 0)
    (raise-arguments-error 'gh-json
                           "gh_helpers call failed"
                           "method"
                           method
                           "path"
                           path
                           "stderr"
                           stderr))
  ;; h.init() prints a short banner; the JSON body is the last non-empty line.
  (define lines
    (filter (lambda (line) (not (equal? (string-trim line) ""))) (string-split stdout "\n")))
  (string->jsexpr (if (pair? lines)
                      (last lines)
                      "null")))

;; Trigger CI on a branch via workflow_dispatch (the approved mechanism: a
;; plain push to a non-main branch does NOT start the workflow).
(define (workflow-dispatch! repo-root branch #:workflow [workflow "ci.yml"])
  (gh-json repo-root "POST" (format "actions/workflows/~a/dispatches" workflow) (hash 'ref branch))
  (void))

(define (find-run-for-sha repo-root sha)
  (define body (gh-json repo-root "GET" (format "actions/runs?head_sha=~a&per_page=20" sha)))
  (run-for-sha (hash-ref body 'workflow_runs '()) sha))

(define (wait-for-run repo-root
                      sha
                      #:timeout-seconds [timeout-seconds 2400]
                      #:poll-seconds [poll-seconds 20])
  (let loop ([waited 0])
    (define run (find-run-for-sha repo-root sha))
    (cond
      [(and run (equal? (hash-ref run 'status "") "completed")) run]
      [(>= waited timeout-seconds) #f]
      [else
       (sleep poll-seconds)
       (loop (+ waited poll-seconds))])))

(define (jobs-for-run repo-root run-id)
  (hash-ref (gh-json repo-root "GET" (format "actions/runs/~a/jobs?per_page=100" run-id)) 'jobs '()))

;; Exact-SHA verdict: dispatch, wait for the run whose head SHA equals `sha`,
;; then classify. A missing run (dispatch failed to start one) is unavailable.
(define (ci-verdict-for-sha repo-root
                            sha
                            #:branch [branch #f]
                            #:timeout-seconds [timeout-seconds 2400])
  (when branch
    (workflow-dispatch! repo-root branch))
  (define run (wait-for-run repo-root sha #:timeout-seconds timeout-seconds))
  (cond
    [(not run)
     (hash 'verdict
           'unavailable
           'run-status
           'missing
           'head-sha
           sha
           'unavailable-reasons
           (list "no CI run appeared for this exact head SHA within the timeout"))]
    [else (classify-ci-run run (jobs-for-run repo-root (hash-ref run 'id)))]))
