#lang racket
;; @speed fast
;; @suite workflows
;; @boundary integration
;; @timeout 300
;; The cold `raco test -t` compile of the qt/full-plan closure takes ~117 s
;; locally; the runner's 120 s default left no headroom on a loaded shard.
;; tests/test-full-regression-invocation.rkt — W1 (#9949) workflow/CLI
;; invocation contract.
;;
;; Incident: full-regression run 44286441007 failed all six Linux shards and
;; the aggregation because the workflow called bare `./qt --suite ...`. A
;; bare `./qt` with flags and no `--lane` dispatches to the default PR lane,
;; which refuses CI-surface flags fail-closed:
;;   "qt: default does not accept CI-surface option --suite
;;    (only ./qt full / ./qt --lane NAME do)"
;; (The Nov 15 run 43708558326 additionally proved the run-level damage of
;; that class: it executed only `mutation-pilot` while every real regression
;; job was skipped.)
;;
;; This test pins the repair at BOTH layers without recursively executing a
;; broad suite:
;;   1. Workflow text — the full-regression shard step invokes the explicit
;;      `./qt full --suite ...` surface, and NO workflow file contains the
;;      stale bare spelling.
;;   2. CLI grammar — the exact matrix option combination (--suite, --jobs,
;;      --shard-index, --shard-total, --profile, --ledger, --json-out) is
;;      accepted by the qt spec validation / plan resolution, while the stale
;;      bare spelling, the --suite + --lane combination and the
;;      --explain + output-flag combination fail closed.
;;
;; Nothing here executes a suite: plan resolution only classifies/selects,
;; and the `--explain` probes execute nothing by contract. The same warranty
;; as tests/test-ci-workflows.rkt: drift fails a fast suite instead of being
;; discovered by a nightly incident.

(require racket/match
         rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "../scripts/testing/full-plan.rkt")

(define-runtime-path here "test-full-regression-invocation.rkt")
(define repo-root (simplify-path (build-path (path-only here) "..")))
(define workflows-dir (build-path repo-root ".github" "workflows"))
(define fr-path (build-path workflows-dir "full-regression.yml"))
(define ci-path (build-path workflows-dir "ci.yml"))

;; Collapse every whitespace run and resolve shell line-continuations (a
;; backslash immediately before a newline joins the two lines), so the
;; assertions see the invocation the shell sees, not the YAML wrapping.
(define (normalized text)
  (string-join (string-split (string-replace text "\\\n" " ")) " "))

;; The invocation text without YAML/shell comment lines: the positive surface
;; assertions match the executable text, and explanatory prose in comments
;; must never satisfy or trip an invocation assertion.
(define (comment-stripped text)
  (string-join (for/list ([l (in-list (string-split text "\n"))]
                          #:unless (string-prefix? (string-trim l) "#"))
                 l)
               "\n"))

(define (workflow-yml-paths)
  (for/list ([p (in-list (directory-list workflows-dir))]
             #:when (regexp-match? #rx"\\.ya?ml$" (path->string p)))
    (build-path workflows-dir p)))

;; The lines of one workflow step: from its "- name:" line to (excluding)
;; the next step at the same indentation.
(define (step-lines text step-name)
  (define lines (string-split text "\n"))
  (define start
    (for/first ([l (in-list lines)]
                [i (in-naturals)]
                #:when (string-prefix? l (string-append "      - name: " step-name)))
      i))
  (and start
       (let loop ([rest (list-tail lines start)])
         (cond
           [(null? (cdr rest)) (list (car rest))]
           [(string-prefix? (cadr rest) "      - name:") (list (car rest))]
           [else (cons (car rest) (loop (cdr rest)))]))))

;; ---------------------------------------------------------------
;; 1. The full-regression shard step uses the explicit full surface
;; ---------------------------------------------------------------

(define-test-suite
 workflow-text-contract
 (test-case "the full-regression shard step invokes ./qt full --suite (never bare ./qt --suite)"
   (define wf (file->string fr-path))
   (define ls (step-lines wf "Run shard"))
   (check-true (and ls #t) "the main matrix 'Run shard' step must exist")
   (when ls
     (define step (normalized (comment-stripped (string-join ls "\n"))))
     ;; The repaired surface (run 44286441007's six shards failed on exactly
     ;; this spelling being absent).
     (check-true (string-contains? step "STRICT_TEST_RUNNER=1 ./qt full --suite \"$SUITE\"")
                 "the shard step must invoke the explicit `./qt full --suite` surface")
     ;; The incident class, forbidden everywhere in the step.
     (check-false
      (string-contains? step "./qt --suite")
      "bare `./qt --suite` dispatches to the default PR lane, which refuses CI-surface flags")
     ;; A lane is a suite union; the CI matrix owns the shard via --shard-*.
     (check-false (string-contains? step "--lane")
                  "the shard matrix must select the suite itself, not through --lane")
     ;; The complete matrix option combination stays present.
     (check-true (string-contains? step "--jobs \"$JOBS\""))
     (check-true (string-contains? step "--shard-index"))
     (check-true (string-contains? step "--shard-total 6"))
     (check-true (string-contains? step "--profile \"$PROFILE\""))
     (check-true (string-contains? step "--json-out shard-results/"))
     (check-true (string-contains? step "--ledger .github/known-failures.json"))
     ;; The ledger is guarded: an absent known-failures file is not an error.
     (check-true (string-contains? step "if [ -f .github/known-failures.json ]"))))
 (test-case "no workflow file contains the stale bare ./qt --suite invocation"
   (define offenders
     (for/list ([p (in-list (workflow-yml-paths))]
                #:when (string-contains? (normalized (comment-stripped (file->string p)))
                                         "./qt --suite"))
       (path->string p)))
   (check-equal? offenders
                 '()
                 (format "bare ./qt --suite found in: ~a" (string-join offenders ", "))))
 (test-case "history-dependent full-regression jobs use a full checkout"
   (define lines (string-split (file->string fr-path) "\n"))
   (define (job-section name)
     (define start
       (for/first ([l (in-list lines)]
                   [i (in-naturals)]
                   #:when (string-prefix? l (format "  ~a:" name)))
         i))
     (and start
          (let loop ([rest (list-tail lines start)])
            (cond
              [(null? (cdr rest)) (list (car rest))]
              [(regexp-match? #px"^  [a-zA-Z][a-zA-Z0-9_-]*:\\s*$" (cadr rest)) (list (car rest))]
              [else (cons (car rest) (loop (cdr rest)))]))))
   (for ([job (in-list '("test" "workflows-suite"))])
     (define section (job-section job))
     (check-true (and section #t) (format "~a job must exist" job))
     (when section
       (define text (normalized (comment-stripped (string-join section "\n"))))
       (check-true (string-contains? text "- uses: actions/checkout@v7 with: fetch-depth: 0")
                   (format "~a checkout must fetch full history" job))))))

;; ---------------------------------------------------------------
;; 2. The Racket 8.11 compatibility lane lives here (weekly + manual)
;; ---------------------------------------------------------------

;; W1 (#9949): the per-push ci.yml job was removed; the verification itself
;; is retained in full-regression.yml with the same version identity, a real
;; fast-lane invocation and retained log/JSON evidence.
(define weekly-cron "40 2 * * 6")

;; One qt dispatcher probe. `racket qt <argv>` runs the same dispatcher the
;; workflows hit as `./qt <argv>`; the cwd is the repo root so relative path
;; spellings resolve as in CI. Returns (list exit-code stdout stderr).
;; Probes use --explain (which executes nothing) or expect a refusal.
(define (qt-argv arguments)
  (define exe (find-executable-path "racket"))
  (define qt (path->string (build-path repo-root "qt")))
  (parameterize ([current-directory repo-root])
    ;; subprocess returns four values: the process, then the child's stdout
    ;; (an input port here), its stdin (the only output port this side may
    ;; close) and its stderr.
    (define-values (proc stdout stdin stderr) (apply subprocess #f #f #f exe qt arguments))
    (close-output-port stdin)
    (define stdout-str (port->string stdout))
    (define stderr-str (port->string stderr))
    (subprocess-wait proc)
    (close-input-port stdout)
    (close-input-port stderr)
    (list (subprocess-status proc) stdout-str stderr-str)))

(define-test-suite
 compat-lane-contract
 (test-case "cross-version-compat job: weekly schedule + dispatch, real lane, retained evidence"
   (define wf (file->string fr-path))
   (define job-start
     (for/first ([l (in-list (string-split wf "\n"))]
                 [i (in-naturals)]
                 #:when (string-prefix? l "  cross-version-compat:"))
       i))
   (check-true (and job-start #t) "full-regression.yml must define the cross-version-compat job")
   (when job-start
     (define job
       (normalized (string-join (let loop ([ls (list-tail (string-split wf "\n") job-start)])
                                  (cond
                                    [(null? (cdr ls)) (list (car ls))]
                                    [(regexp-match? #rx"^  [a-zA-Z][a-zA-Z0-9_-]*:\\s*$" (cadr ls))
                                     (list (car ls))]
                                    [else (cons (car ls) (loop (cdr ls)))]))
                                "\n")))
     ;; Cadence: the weekly cron plus the matching schedule guard, and
     ;; manual dispatch is always allowed.
     (check-true (string-contains? wf (format "cron: \"~a\"" weekly-cron))
                 "the weekly compatibility cron must be declared")
     (check-true (string-contains? job (format "github.event.schedule == '~a'" weekly-cron))
                 "the job must fire on the weekly cron only (the nightly schedule must not run it)")
     (check-true (string-contains? job "github.event_name == 'workflow_dispatch'")
                 "manual dispatch must run the compatibility lane")
     ;; Version identity + a real fast-lane invocation (never a token probe).
     (check-true (string-contains? job "racket-version: \"8.11\"")
                 "the compatibility lane must pin Racket 8.11")
     (check-true (string-contains? job "STRICT_TEST_RUNNER=1 ./qt full --lane fast")
                 "the lane must run the real fast suite through the explicit full command")
     ;; Retained log/JSON evidence.
     (check-true (string-contains? job "results-cross-version-811"))
     (check-true (string-contains? job "cross-version-811.json"))
     (check-true (string-contains? job "retention-days: 30"))
     (check-true (string-contains? job "if: always()")
                 "evidence must be uploaded even on a red compatibility run")))
 (test-case "cross-version-compat is independent of the definitive full-regression status"
   (define wf (file->string fr-path))
   ;; The summarize job aggregates the six Linux shards + workflows + platform
   ;; lanes only; the compatibility lane must never weaken or substitute for
   ;; that evidence.
   (check-true (string-contains? wf "needs: [test, workflows-suite, test-platform]")
               "summarize must keep exactly its three required-lane producers")
   (define summarize-lines
     (for/list ([l (in-list (string-split wf "\n"))]
                [i (in-naturals)]
                #:when (string-contains? l "needs: [test, workflows-suite, test-platform]"))
       i))
   (check-true (= (length summarize-lines) 1)))
 (test-case "ci.yml no longer runs a Racket 8.11 cross-version job on push"
   (define text (file->string ci-path))
   (check-false (regexp-match? #px"(?m:^  test-cross-version:)" text)
                "the per-push test-cross-version job must be removed from ci.yml")
   (check-false (string-contains? text "racket-version: '8.11'")
                "ci.yml must not install Racket 8.11 anymore")))

;; ---------------------------------------------------------------
;; 3. The qt grammar accepts the exact matrix combination
;; ---------------------------------------------------------------

(define-test-suite
 qt-grammar-contract
 ;; In-process plan resolution: selection/classification only, nothing is
 ;; executed or cleaned (resolve-qt-ci-plan has no execution side effects).
 (test-case "the CI matrix option combination resolves to a valid plan"
   (define ledger (make-temporary-file "w1-ledger~a.json"))
   (call-with-output-file ledger (lambda (o) (display "[]" o)) #:exists 'truncate)
   (define json-out (path->string (make-temporary-file "w1-json-out~a.json" #f)))
   (define plan
     (resolve-qt-ci-plan
      #f
      (qt-ci-spec 'all '() 5 6 #f 'ci (path->string ledger) json-out #f 1 'auto #f #f '())))
   (check-equal? (qt-ci-plan-suite plan) 'all)
   (check-equal? (qt-ci-plan-shard-index plan) 5)
   (check-equal? (qt-ci-plan-shard-total plan) 6)
   (check-equal? (qt-ci-plan-mode plan) 'explicit-suite)
   (check-equal? (qt-ci-spec-json-out (qt-ci-plan-spec plan))
                 json-out
                 "--json-out is accepted by the grammar and carried on the spec")
   (check-true (pair? (qt-ci-plan-rels plan)) "shard 5 of 6 must select a non-empty file slice")
   (delete-file ledger))
 ;; Subprocess probes pin the fail-closed surface exactly as CI observes it.
 ;; Each probe either explains (executes nothing) or must refuse; the probe
 ;; helper `qt-argv` is defined at module level above.
 (test-case "the stale bare ./qt --suite spelling is refused (run 44286441007's failure)"
   (match-define (list code out err) (qt-argv '("--suite" "all" "--explain")))
   (check-false (zero? code) "bare ./qt --suite must fail closed")
   (check-true (string-contains? err "default does not accept CI-surface option --suite")
               "the refusal must name the incident message and the supported surface")
   (check-false (string-contains? out "selection:") "a refusal must not print a plan"))
 (test-case "--suite cannot be combined with --lane (the lane owns the suite)"
   (match-define (list code _out err) (qt-argv '("full" "--lane" "fast" "--suite" "all" "--explain")))
   (check-false (zero? code))
   (check-true (string-contains? err "--lane cannot be combined with --suite")))
 (test-case "--explain refuses output-producing flags (fail closed, not ignored)"
   (match-define (list code _out err)
     (qt-argv '("full" "--suite" "all" "--json-out" "w1-probe.json" "--explain")))
   (check-false (zero? code))
   (check-true (string-contains? err "--explain cannot be combined with --json-out")))
 (test-case "the explicit ./qt full --suite matrix combination plans cleanly"
   ;; --explain executes nothing: this is the validated shard-0 shape of the
   ;; repaired workflow step, without running any test.
   (match-define (list code out _err)
     (qt-argv '("full" "--suite"
                       "all"
                       "--jobs"
                       "4"
                       "--shard-index"
                       "0"
                       "--shard-total"
                       "6"
                       "--profile"
                       "ci"
                       "--explain")))
   (check-true (zero? code) "the repaired combination must be accepted")
   (check-true (string-contains? out "shard 0 of 6"))
   (check-true (string-contains? out "suite: all")))
 (test-case "the matrix combination accepts --ledger and the last shard index"
   (define ledger (make-temporary-file "w1-probe-ledger~a.json"))
   (call-with-output-file ledger (lambda (o) (display "[]" o)) #:exists 'truncate)
   (match-define (list code out _err)
     (qt-argv (list "full"
                    "--suite"
                    "all"
                    "--jobs"
                    "4"
                    "--shard-index"
                    "5"
                    "--shard-total"
                    "6"
                    "--profile"
                    "ci"
                    "--ledger"
                    (path->string ledger)
                    "--explain")))
   (delete-file ledger)
   (check-true (zero? code) "an existing ledger file must be accepted")
   (check-true (string-contains? out "shard 5 of 6"))))

(module+ test
  (require rackunit/text-ui)
  (exit (run-tests (test-suite "tests/test-full-regression-invocation.rkt"
                     workflow-text-contract
                     compat-lane-contract
                     qt-grammar-contract))))
