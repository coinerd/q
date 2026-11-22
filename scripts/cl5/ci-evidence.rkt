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
         unavailable-conclusions
         workflow-dispatch!
         find-run-for-sha
         wait-for-run
         jobs-for-run
         ci-verdict-for-sha
         scratch-branch-name)

;; Conclusions that mean "the comprehensive surface did not run".
(define unavailable-conclusions '(cancelled timed_out action_required stale))

;; The required test producers described in the review: the authoritative
;; comprehensive surface for a CI run. Lint/audit jobs are recorded too, but a
;; skipped lint job does not by itself invalidate detection evidence.
(define required-producers
  '("test (0)" "test (1)"
               "test (2)"
               "test-aggregate"
               "security"
               "test-platform"
               "smoke (ubuntu-latest)"
               "workflows (0)"
               "workflows (1)"))

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
      (hash 'name
            (job-name job)
            'conclusion
            (hash-ref job 'conclusion #f)
            'failed-steps
            (for/list ([step (in-list (hash-ref job 'steps '()))]
                       #:when (equal? (hash-ref step 'conclusion #f) "failure"))
              (hash-ref step 'name "")))))
  (define unavailable-reasons
    (append
     (cond
       [(not (equal? status "completed")) (list (format "run status is ~a, not completed" status))]
       [(memq (and conclusion (string->symbol conclusion)) unavailable-conclusions)
        (list (format "run conclusion is ~a" conclusion))]
       [else '()])
     (for/list ([summary (in-list job-summaries)]
                #:when (and (member (hash-ref summary 'name) required-producers)
                            (memq (let ([c (hash-ref summary 'conclusion #f)])
                                    (and (string? c) (string->symbol c)))
                                  '(skipped cancelled timed_out action_required))))
       (format "required producer ~a was ~a"
               (hash-ref summary 'name)
               (hash-ref summary 'conclusion)))))
  (define failed
    (for/list ([summary (in-list job-summaries)]
               #:when (equal? (hash-ref summary 'conclusion) "failure"))
      summary))
  (hash 'verdict
        (cond
          [(pair? unavailable-reasons) 'unavailable]
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
        'detecting
        failed
        'unavailable-reasons
        unavailable-reasons))

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
             (list "-c" code method path (if data (jsexpr->string data) ""))))
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
  (define lines (filter (lambda (line) (not (equal? (string-trim line) "")))
                        (string-split stdout "\n")))
  (string->jsexpr (if (pair? lines) (last lines) "null")))

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
