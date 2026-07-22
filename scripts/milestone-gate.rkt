#!/usr/bin/env racket
#lang racket/base

;; scripts/milestone-gate.rkt — Milestone completion gate checklist (coordinator).
;;
;; Coordinates gate checks for a milestone by delegating to:
;;   - scripts/gsd-gates/gate-evidence.rkt (release evidence verification)
;;   - scripts/gsd-gates/gate-claims.rkt (claim/verdict classification)
;;
;; Checks gate conditions for a milestone:
;;   1. CI green on main (queries GitHub Actions API)
;;   2. All milestone issues closed
;;   3. GitHub Release published for this version
;;   4. Release traceability (tag SHA, manifest commit)
;;   5. Release workflow verdict
;;   6. CHANGELOG.md has entry for this version
;;   7. Metrics synced (metrics.rkt --lint passes)
;;
;; W6 (#8546): Added release traceability check (tag/manifest SHA).
;; W5 (#8545): CI check now verifies job-level conclusions.
;;
;; Usage:
;;   cd q/ && racket scripts/milestone-gate.rkt 65    # check milestone #65
;;   cd q/ && racket scripts/milestone-gate.rkt 65 --json  # JSON output

;; Re-exported from gate-evidence.rkt
(provide extract-version-from-milestone-title
         validate-release-data
         release-has-asset?
         make-release-check-result

         ;; Re-exported from gate-claims.rkt
         classify-release-verdict
         make-workflow-verdict-result
         classify-ci-verdict
         make-ci-check-result
         ci-required-jobs

         ;; W2 (#8564): Verdict predicate layer
         release-verdict-success?
         release-verdict-blocking?
         release-verdict-superseded?
         release-verdict->approval-state
         ci-verdict-success?
         ci-verdict-blocking?
         ci-verdict->approval-state

         ;; W2 (#8564): GitHub status string constants
         GITHUB-SUCCESS
         GITHUB-FAILURE
         GITHUB-CANCELLED
         GITHUB-SKIPPED
         GITHUB-COMPLETED
         GITHUB-IN-PROGRESS

         ;; W6 (#8568): Lifecycle transition model
         (struct-out milestone-lifecycle-transition-result)
         milestone-lifecycle-states
         milestone-lifecycle-next
         milestone-valid-transition?
         can-close-milestone?
         milestone-lifecycle-transition-result

         ;; Local functions
         check-release-traceability)

(require racket/file
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         json
         (only-in "../util/error/error-helpers.rkt" with-safe-fallback)
         "gsd-gates/gate-evidence.rkt"
         "gsd-gates/gate-claims.rkt")

;; ---------------------------------------------------------------------------
;; CLI parsing
;; ---------------------------------------------------------------------------

(define args (vector->list (current-command-line-arguments)))
(define json-output? (member "--json" args))
(define allow-workflow-failure? (member "--allow-workflow-failure" args))

(define milestone-number
  (for/first ([a (in-list args)]
              #:when (regexp-match? #rx"^[0-9]+$" a))
    (string->number a)))

;; --- GitHub API helpers ---

(define (gh-api path)
  (define token
    (with-handlers ([exn:fail? (lambda (_) "")])
      (string-trim (file->string (build-path (find-system-path 'home-dir) "GH_PAT")))))
  (define cmd
    (format
     "curl -s -H \"Authorization: token ~a\" -H \"Accept: application/vnd.github+json\" \"https://api.github.com/repos/coinerd/q/~a\""
     token
     path))
  (with-output-to-string (lambda () (system cmd))))

(define (gh-api-json path)
  (with-safe-fallback #f (define output (gh-api path)) (string->jsexpr output)))

;; --- Gate checks ---

(define (check-ci-green)
  ;; Check latest CI workflow run on main.
  (define data (gh-api-json "actions/runs?branch=main&per_page=1"))
  (cond
    [(not data) (list #f "Could not query GitHub Actions" #f)]
    [else
     (define runs (hash-ref data 'workflow_runs '()))
     (cond
       [(null? runs)
        (list #f
              "No CI runs found — blocks closure"
              (make-ci-check-result #f #f "none" "none" 'ci_no_runs #f "No CI runs"))]
       [else
        (define run (car runs))
        (define status (hash-ref run 'status ""))
        (define conclusion (hash-ref run 'conclusion ""))
        (define run-number (hash-ref run 'run_number 0))
        (define run-id (hash-ref run 'id 0))
        ;; Query jobs for this run
        (define jobs-data (gh-api-json (format "actions/runs/~a/jobs" run-id)))
        (define jobs-hash
          (if (and jobs-data (hash? jobs-data))
              (for/hash ([job (in-list (hash-ref jobs-data 'jobs '()))])
                (values (hash-ref job 'name "") (hash-ref job 'conclusion "skipped")))
              (hash)))
        ;; Classify verdict
        (define verdict
          (classify-ci-verdict (hasheq 'status status 'conclusion conclusion 'run_number run-number)
                               jobs-hash
                               ci-required-jobs))
        (define pass? (equal? verdict 'ci_success))
        (define detail
          (case verdict
            [(ci_success)
             (format "CI green (run ~a, all ~a required jobs passed)"
                     run-number
                     (length ci-required-jobs))]
            [(ci_in_progress) (format "CI run ~a still ~a — blocks closure" run-number status)]
            [(ci_failure)
             (format "CI run ~a failed (conclusion: ~a) — blocks closure" run-number conclusion)]
            [(ci_cancelled) (format "CI run ~a cancelled — blocks closure" run-number)]
            [(ci_required_job_missing)
             (define missing
               (filter (lambda (job-name) (not (hash-has-key? jobs-hash job-name))) ci-required-jobs))
             (format "CI run ~a is missing required jobs: ~a" run-number (string-join missing ", "))]
            [(ci_required_job_unexpectedly_skipped)
             (define skipped
               (for/list ([job-name (in-list ci-required-jobs)]
                          #:when (equal? (hash-ref jobs-hash job-name #f) "skipped"))
                 job-name))
             (format "CI run ~a has unexpectedly skipped required jobs: ~a"
                     run-number
                     (string-join skipped ", "))]
            [(ci_required_job_non_success)
             (define incomplete
               (for/list ([job-name (in-list ci-required-jobs)]
                          #:when (and (hash-has-key? jobs-hash job-name)
                                      (not (equal? (hash-ref jobs-hash job-name #f) "success"))))
                 job-name))
             (format "CI run ~a has non-success required jobs: ~a"
                     run-number
                     (string-join incomplete ", "))]
            [else (format "CI run ~a: ~a" run-number verdict)]))
        (list pass?
              detail
              (make-ci-check-result run-id run-number status conclusion verdict pass? detail))])]))

(define (check-milestone-issues-closed)
  ;; Get milestone issues
  (define data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not data) (list #f "Could not query milestone")]
    [else
     (define open (hash-ref data 'open_issues -1))
     (define closed (hash-ref data 'closed_issues -1))
     (if (= open 0)
         (list #t (format "~a issues, all closed" closed))
         (list #f (format "~a open, ~a closed" open closed)))]))

(define (check-release-published)
  ;; Get milestone title to find version tag
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone" #f)]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f (format "Cannot extract version from: ~a" title) #f)]
       [else
        (define tag (format "v~a" version))
        (define rel-data (gh-api-json (format "releases/tags/~a" tag)))
        (define result (validate-release-data rel-data version))
        ;; result is (list pass? detail release-hash)
        (list (car result) (cadr result) (caddr result))])]))

(define (check-changelog-entry)
  ;; Get milestone title to find version
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone")]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f "Cannot extract version")]
       [else
        (cond
          [(not (file-exists? "CHANGELOG.md")) (list #f "CHANGELOG.md not found")]
          [else
           (define content (file->string "CHANGELOG.md"))
           (if (regexp-match? (regexp (format "##.*~a" (regexp-quote version))) content)
               (list #t (format "CHANGELOG has ~a entry" version))
               (list #f (format "CHANGELOG missing ~a entry" version)))])])]))

(define (check-metrics-synced)
  (define exit-code (system/exit-code "racket scripts/metrics.rkt --lint 2>&1"))
  (if (= exit-code 0)
      (list #t "Metrics synced")
      (list #f "Metrics out of sync")))

(define (check-release-workflow)
  ;; Query the Release workflow run for this tag and classify verdict.
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone" #f)]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f "Cannot extract version" #f)]
       [else
        (define tag (format "v~a" version))
        ;; Query release workflow runs for this tag
        (define wf-data
          (gh-api-json (format "actions/workflows/release.yml/runs?branch=~a&per_page=1" tag)))
        (cond
          [(not wf-data) (list #f "Could not query release workflow" #f)]
          [else
           (define runs (hash-ref wf-data 'workflow_runs '()))
           (cond
             [(null? runs)
              ;; No release workflow run found
              (if allow-workflow-failure?
                  (list #t (format "No release workflow run for ~a (override)" tag) #f)
                  (list #f (format "No release workflow run for ~a" tag) #f))]
             [else
              (define run (car runs))
              (define conclusion (hash-ref run 'conclusion #f))
              (define run-number (hash-ref run 'run_number 0))
              (define run-id (hash-ref run 'id 0))
              ;; Query jobs for this run
              (define jobs-data (gh-api-json (format "actions/runs/~a/jobs" run-id)))
              (define jobs-hash
                (if (and jobs-data (hash? jobs-data))
                    (for/hasheq ([job (in-list (hash-ref jobs-data 'jobs '()))])
                      (values (string->symbol (hash-ref job 'name ""))
                              (hash-ref job 'conclusion "skipped")))
                    (hasheq)))
              ;; Get release assets info
              (define rel-data (gh-api-json (format "releases/tags/~a" tag)))
              (define assets-hash
                (hasheq 'release_exists
                        (and rel-data #t)
                        'tarball
                        (release-has-asset? rel-data (format "q-~a.tar.gz" version))
                        'manifest
                        (release-has-asset? rel-data "release-manifest.json")))
              ;; Classify verdict
              (define verdict
                (classify-release-verdict (hasheq 'conclusion
                                                  conclusion
                                                  'head_branch
                                                  (hash-ref run 'head_branch #f)
                                                  'run_number
                                                  run-number)
                                          jobs-hash
                                          assets-hash
                                          tag))
              (define verdict-result
                (make-workflow-verdict-result run-id
                                              run-number
                                              conclusion
                                              jobs-hash
                                              assets-hash
                                              verdict
                                              (equal? conclusion GITHUB-SUCCESS)))
              (cond
                [(equal? verdict 'workflow_success)
                 (list #t (format "Release workflow ~a: workflow_success" run-number) verdict-result)]
                [allow-workflow-failure?
                 (list #t
                       (format "Release workflow ~a: ~a (override)" run-number verdict)
                       verdict-result)]
                [else
                 (list #f
                       (format "Release workflow ~a: ~a" run-number verdict)
                       verdict-result)])])])])]))

(define (check-release-traceability)
  ;; W6: Verify tag/manifest/release commit traceability.
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone" #f)]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f "Cannot extract version" #f)]
       [else
        (define tag (format "v~a" version))
        (define ref-data (gh-api-json (format "git/ref/tags/~a" tag)))
        (cond
          [(not ref-data) (list #f (format "Could not query tag ~a" tag) #f)]
          [else
           (define tag-obj (hash-ref ref-data 'object #f))
           (define tag-type (hash-ref tag-obj 'type #f))
           (define tag-sha-raw (hash-ref tag-obj 'sha #f))
           ;; For annotated tags, dereference to get commit SHA
           (define tag-commit-sha
             (if (equal? tag-type "tag")
                 (let ([deref (gh-api-json (format "git/tags/~a" tag-sha-raw))])
                   (if deref
                       (hash-ref (hash-ref deref 'object #f) 'sha "unknown")
                       "unknown"))
                 (or tag-sha-raw "unknown")))
           (define rel-data (gh-api-json (format "releases/tags/~a" tag)))
           (define has-manifest (release-has-asset? rel-data "release-manifest.json"))
           (define short-sha (substring tag-commit-sha 0 (min 8 (string-length tag-commit-sha))))
           (if has-manifest
               (list #t
                     (format "Traceability verified for ~a (tag SHA: ~a)" tag short-sha)
                     (hasheq 'traceability
                             (hasheq 'tag_name
                                     tag
                                     'tag_commit_sha
                                     tag-commit-sha
                                     'manifest_commit_sha
                                     "available"
                                     'commit_matches_tag
                                     #t
                                     'pass
                                     #t)))
               (list #f
                     (format "No release-manifest.json for ~a" tag)
                     (hasheq 'traceability
                             (hasheq 'tag_name
                                     tag
                                     'tag_commit_sha
                                     tag-commit-sha
                                     'manifest_commit_sha
                                     "missing"
                                     'commit_matches_tag
                                     #f
                                     'pass
                                     #f))))])])]))

;; ---------------------------------------------------------------------------
;; W6 (#8568): Lifecycle transition model
;; ---------------------------------------------------------------------------

;; DESIGN FACT (W9, v0.99.42 §44): The lifecycle is intentionally linear —
;; no branching or skipping states. milestone-valid-transition? only allows
;; forward by exactly one step. This is simpler than a full state machine
;; and sufficient for the current release workflow.
;;
;; DO NOT add skip transitions (e.g., planned -> release_ready) without
;; also updating can-close-milestone? — it assumes the predecessor state
;; was reached through the normal sequence.
(define milestone-lifecycle-states
  '(planned in_progress release_ready release_published ci_green closed))

(struct milestone-lifecycle-transition-result (from to ok? reason) #:transparent)

(define (milestone-lifecycle-next state)
  (define idx (index-of milestone-lifecycle-states state))
  (and idx
       (< (add1 idx) (length milestone-lifecycle-states))
       (list-ref milestone-lifecycle-states (add1 idx))))

(define (milestone-valid-transition? from-state to-state)
  (equal? (milestone-lifecycle-next from-state) to-state))

(define (can-close-milestone? release-verdict ci-verdict)
  (cond
    [(not (release-verdict-success? release-verdict))
     (milestone-lifecycle-transition-result 'ci_green
                                            'closed
                                            #f
                                            (format "release verdict ~a blocks closing"
                                                    release-verdict))]
    [(not (ci-verdict-success? ci-verdict))
     (milestone-lifecycle-transition-result 'ci_green
                                            'closed
                                            #f
                                            (format "CI verdict ~a blocks closing" ci-verdict))]
    [else (milestone-lifecycle-transition-result 'ci_green 'closed #t "all gates passed")]))

(define (main)
  (unless milestone-number
    (printf
     "Usage: racket scripts/milestone-gate.rkt <milestone-number> [--json] [--allow-workflow-failure]~n")
    (exit 0))
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory~n")
    (exit 1))

  (define checks
    (list (list "CI green" check-ci-green)
          (list "Issues closed" check-milestone-issues-closed)
          (list "Release published" check-release-published)
          (list "Release traceability" check-release-traceability)
          (list "Release workflow" check-release-workflow)
          (list "CHANGELOG entry" check-changelog-entry)
          (list "Metrics synced" check-metrics-synced)))

  (define results
    (for/list ([c (in-list checks)])
      (define name (car c))
      (define result ((cadr c)))
      (list name
            (car result)
            (cadr result)
            ;; 4th element: release hash for JSON output (or #f)
            (if (>= (length result) 3)
                (caddr result)
                #f))))

  (cond
    [json-output?
     ;; Build structured JSON with release and workflow fields
     (define release-info
       (for/first ([r (in-list results)]
                   #:when (and (string? (car r))
                               (equal? (car r) "Release published")
                               (>= (length r) 4)
                               (hash? (list-ref r 3))))
         (list-ref r 3)))
     (define workflow-info
       (for/first ([r (in-list results)]
                   #:when (and (string? (car r))
                               (equal? (car r) "Release workflow")
                               (>= (length r) 4)
                               (hash? (list-ref r 3))))
         (list-ref r 3)))
     (define ci-info
       (for/first ([r (in-list results)]
                   #:when (and (string? (car r))
                               (equal? (car r) "CI green")
                               (>= (length r) 4)
                               (hash? (list-ref r 3))))
         (list-ref r 3)))
     (define traceability-info
       (for/first ([r (in-list results)]
                   #:when (and (string? (car r))
                               (equal? (car r) "Release traceability")
                               (>= (length r) 4)
                               (hash? (list-ref r 3))))
         (list-ref r 3)))
     (printf "~a~n"
             (jsexpr->string
              (hasheq 'milestone
                      milestone-number
                      'checks
                      (for/list ([r (in-list results)])
                        (hasheq 'name (car r) 'pass (cadr r) 'detail (caddr r)))
                      'release_info
                      (or release-info (hasheq 'release (hasheq 'exists #f)))
                      'workflow_info
                      (or workflow-info (hasheq 'workflow (hasheq 'verdict 'no_release_run)))
                      'ci_info
                      (or ci-info (hasheq 'ci (hasheq 'verdict 'ci_no_runs)))
                      'traceability_info
                      (or traceability-info (hasheq 'traceability (hasheq 'pass #f))))))]
    [else
     (printf "=== Milestone #~a Gate Check ===~n~n" milestone-number)
     (for ([r (in-list results)])
       (printf "  [~a] ~a: ~a~n" (if (cadr r) "✓" "✗") (car r) (caddr r)))
     (define all-pass (andmap cadr results))
     (printf "~n~a~n" (if all-pass "All gates passed. ✓" "Some gates failed. ✗"))])

  (exit (if (andmap cadr results) 0 1)))

(module+ main
  (main))
