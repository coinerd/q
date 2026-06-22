#!/usr/bin/env racket
#lang racket/base

;; scripts/milestone-gate.rkt — Milestone completion gate checklist.
;;
;; Checks 5 gate conditions for a milestone:
;;   1. CI green on main (queries GitHub Actions API)
;;   2. All milestone issues closed
;;   3. GitHub Release published for this version
;;   4. CHANGELOG.md has entry for this version
;;   5. Metrics synced (metrics.rkt --lint passes)
;;
;; W5 (#8545): CI check now verifies job-level conclusions and
;; blocks milestone closure on in-progress, failure, cancelled,
;; or unexpectedly skipped required jobs.
;;
;; Usage:
;;   cd q/ && racket scripts/milestone-gate.rkt 65    # check milestone #65
;;   cd q/ && racket scripts/milestone-gate.rkt 65 --json  # JSON output

(provide extract-version-from-milestone-title
         validate-release-data
         release-has-asset?
         make-release-check-result
         classify-release-verdict
         make-workflow-verdict-result
         classify-ci-verdict
         make-ci-check-result
         ci-required-jobs)

(require racket/file
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         json)
(require (only-in "../util/error/error-helpers.rkt" with-safe-fallback))

;; ---------------------------------------------------------------------------
;; Pure logic (testable without network)
;; ---------------------------------------------------------------------------

;; Build a workflow verdict result hash for JSON output.
(define (make-workflow-verdict-result run-id run-number conclusion jobs-hash assets-hash verdict pass)
  (hasheq 'workflow
          (hasheq 'run_id
                  (or run-id 0)
                  'run_number
                  (or run-number 0)
                  'conclusion
                  (or conclusion "unknown")
                  'pass
                  pass)
          'jobs
          (or jobs-hash (hasheq))
          'release_assets
          (or assets-hash (hasheq 'release_exists #f 'tarball #f 'manifest #f))
          'verdict
          (or verdict "unknown")))

;; Classify a release workflow run into a verdict.
;; Pure function — takes structured data, returns a verdict string.
;;
;; Inputs:
;;   workflow-data: hash or #f
;;     'conclusion → "success" | "failure" | #f (if no run)
;;     'run_number → integer
;;     'head_branch → string (the tag it ran on)
;;   jobs: hash of job-name → conclusion string (e.g. 'test → "success")
;;   assets: hash of 'release_exists 'tarball 'manifest → boolean
;;   expected-tag: string like "v0.99.41"
;;
;; Returns one of:
;;   no_release_run
;;   test_failed_release_skipped
;;   release_failed
;;   publication_succeeded_smoke_failed
;;   workflow_success
;;   stale_run
;;   unknown
(define (classify-release-verdict workflow-data jobs assets expected-tag)
  (cond
    ;; No workflow run at all
    [(not workflow-data) 'no_release_run]
    ;; Stale run — for a different tag
    [(let ([branch (hash-ref workflow-data 'head_branch #f)])
       (and branch expected-tag (not (equal? branch expected-tag))))
     'stale_run]
    [else
     (define wf-conclusion (hash-ref workflow-data 'conclusion #f))
     (define test-status (hash-ref jobs 'test #f))
     (define release-status (hash-ref jobs 'release #f))
     (define smoke-status (hash-ref jobs 'smoke #f))
     (define release-exists (hash-ref assets 'release_exists #f))
     (cond
       ;; All jobs succeeded → workflow success
       [(and (equal? wf-conclusion "success"))
        (if (and (equal? test-status "success")
                 (equal? release-status "success")
                 (or (not smoke-status) (equal? smoke-status "success")))
            'workflow_success
            'unknown)]
       ;; Test failed, release never ran
       [(and (not (equal? test-status "success")) (not release-status)) 'test_failed_release_skipped]
       ;; Release job itself failed
       [(and release-status (not (equal? release-status "success"))) 'release_failed]
       ;; Release succeeded but smoke failed, and assets published
       [(and (equal? test-status "success")
             (equal? release-status "success")
             smoke-status
             (not (equal? smoke-status "success"))
             release-exists)
        'publication_succeeded_smoke_failed]
       [else 'unknown])]))

;; Default required CI jobs for milestone closure.
;; release-dry-run and release-readiness may be skipped on main.
(define ci-required-jobs
  '("lint" "lint-alignment" "test" "security" "smoke" "inter-wave-gate" "workflows"))

;; Build a CI check result hash for JSON output.
(define (make-ci-check-result run-id run-number status conclusion verdict pass detail)
  (hasheq 'ci
          (hasheq 'run_id
                  (or run-id 0)
                  'run_number
                  (or run-number 0)
                  'status
                  (or status "unknown")
                  'conclusion
                  (or conclusion "unknown")
                  'verdict
                  (or verdict 'ci_no_runs)
                  'pass
                  pass
                  'detail
                  (or detail ""))))

;; Classify a CI run for milestone closure purposes.
;; Pure function — takes structured data, returns a verdict symbol.
;;
;; Inputs:
;;   ci-run-data: hash or #f
;;     'status → "completed" | "in_progress" | "queued" | #f
;;     'conclusion → "success" | "failure" | "cancelled" | #f
;;     'head_sha → string
;;     'run_number → integer
;;   jobs: hash of job-name (string) → conclusion string
;;   required-jobs: list of strings (required job names)
;;
;; Returns one of:
;;   ci_no_runs
;;   ci_in_progress
;;   ci_failure
;;   ci_cancelled
;;   ci_required_job_unexpectedly_skipped
;;   ci_success
(define (classify-ci-verdict ci-run-data jobs required-jobs)
  (cond
    [(not ci-run-data) 'ci_no_runs]
    [(not (equal? (hash-ref ci-run-data 'status #f) "completed")) 'ci_in_progress]
    [(equal? (hash-ref ci-run-data 'conclusion #f) "cancelled") 'ci_cancelled]
    [(not (equal? (hash-ref ci-run-data 'conclusion #f) "success")) 'ci_failure]
    [else
     ;; Workflow succeeded — check required jobs for unexpected skips
     (define skipped-required
       (for/list ([job-name (in-list required-jobs)]
                  #:when (equal? (hash-ref jobs job-name #f) "skipped"))
         job-name))
     (if (pair? skipped-required) 'ci_required_job_unexpectedly_skipped 'ci_success)]))

;; Extract version (X.Y.Z) from milestone title like "v0.99.40 ..." or "0.99.40 ..."
;; Returns string or #f.
(define (extract-version-from-milestone-title title)
  (define m (regexp-match #rx"v?([0-9]+\\.[0-9]+\\.[0-9]+)" title))
  (and m (cadr m)))

;; Check if release data has an asset with the given name.
(define (release-has-asset? release-data asset-name)
  (and release-data
       (hash? release-data)
       (let ([assets (hash-ref release-data 'assets '())])
         (for/or ([a (in-list assets)])
           (and (hash? a) (equal? (hash-ref a 'name #f) asset-name))))))

;; Build a release check result hash for JSON output.
(define (make-release-check-result exists tag tarball manifest pass detail)
  (hasheq 'release
          (hasheq 'exists
                  exists
                  'tag
                  (or tag "")
                  'tarball_asset
                  tarball
                  'manifest_asset
                  manifest
                  'pass
                  pass
                  'detail
                  detail)))

;; Validate release data for completeness.
;; Returns (list pass? detail-string release-check-hash).
;; release-data: jsexpr from GitHub API or #f
;; version: string like "0.99.40"
(define (validate-release-data release-data version)
  (define expected-tag (format "v~a" version))
  (define expected-tarball (format "q-~a.tar.gz" version))
  (cond
    ;; No release at all
    [(not release-data)
     (list #f
           (format "No release for ~a" expected-tag)
           (make-release-check-result #f #f #f #f #f (format "No release for ~a" expected-tag)))]
    ;; Release exists but is draft
    [(hash-ref release-data 'draft #f)
     (list #f
           (format "Release ~a is draft" expected-tag)
           (make-release-check-result #t
                                      expected-tag
                                      #f
                                      #f
                                      #f
                                      (format "Release ~a is draft" expected-tag)))]
    ;; Tag mismatch
    [(not (equal? (hash-ref release-data 'tag_name #f) expected-tag))
     (define actual (hash-ref release-data 'tag_name "?"))
     (list #f
           (format "Release tag mismatch: expected ~a, got ~a" expected-tag actual)
           (make-release-check-result #t
                                      actual
                                      #f
                                      #f
                                      #f
                                      (format "Tag mismatch: ~a ≠ ~a" actual expected-tag)))]
    [else
     ;; Check assets
     (define has-tarball (release-has-asset? release-data expected-tarball))
     (define has-manifest (release-has-asset? release-data "release-manifest.json"))
     (define all-ok (and has-tarball has-manifest))
     (define detail
       (cond
         [(and has-tarball has-manifest)
          (format "Release ~a verified: tarball + manifest present" expected-tag)]
         [(and (not has-tarball) (not has-manifest))
          (format "Release ~a missing all assets" expected-tag)]
         [(not has-tarball) (format "Release ~a missing tarball (~a)" expected-tag expected-tarball)]
         [else (format "Release ~a missing manifest" expected-tag)]))
     (list all-ok
           detail
           (make-release-check-result #t expected-tag has-tarball has-manifest all-ok detail))]))

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
    (with-handlers ([exn:fail? (λ (_) "")])
      (string-trim (file->string (build-path (find-system-path 'home-dir) "GH_PAT")))))
  (define cmd
    (format
     "curl -s -H \"Authorization: token ~a\" -H \"Accept: application/vnd.github+json\" \"https://api.github.com/repos/coinerd/q/~a\""
     token
     path))
  (with-output-to-string (λ () (system cmd))))

(define (gh-api-json path)
  (with-safe-fallback #f (define output (gh-api path)) (string->jsexpr output)))

;; --- Gate checks ---

(define (check-ci-green)
  ;; Check latest CI workflow run on main.
  ;; W5: Enriched with job-level verification and required-job skip semantics.
  (define data (gh-api-json "actions/runs?branch=main&per_page=1"))
  (cond
    [(not data) (list #f "Could not query GitHub Actions" #f)]
    [else
     (define runs (hash-ref data 'workflow_runs '()))
     (cond
       [(null? runs)
        (list #t
              "No CI runs found (pass)"
              (make-ci-check-result #f #f "none" "none" 'ci_no_runs #t "No CI runs"))]
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
            [(ci_required_job_unexpectedly_skipped)
             (define skipped
               (for/list ([job-name (in-list ci-required-jobs)]
                          #:when (equal? (hash-ref jobs-hash job-name #f) "skipped"))
                 job-name))
             (format "CI run ~a has unexpectedly skipped required jobs: ~a"
                     run-number
                     (string-join skipped ", "))]
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
  ;; Fails if the Release workflow conclusion is not 'success',
  ;; unless --allow-workflow-failure override is passed.
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
                                              (equal? conclusion "success")))
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

;; --- Main ---

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
     (printf "~a~n"
             (jsexpr->string (hasheq 'milestone
                                     milestone-number
                                     'checks
                                     (for/list ([r (in-list results)])
                                       (hasheq 'name (car r) 'pass (cadr r) 'detail (caddr r)))
                                     'release_info
                                     (or release-info (hasheq 'release (hasheq 'exists #f)))
                                     'workflow_info
                                     (or workflow-info
                                         (hasheq 'workflow (hasheq 'verdict 'no_release_run)))
                                     'ci_info
                                     (or ci-info (hasheq 'ci (hasheq 'verdict 'ci_no_runs))))))]
    [else
     (printf "=== Milestone #~a Gate Check ===~n~n" milestone-number)
     (for ([r (in-list results)])
       (printf "  [~a] ~a: ~a~n" (if (cadr r) "✓" "✗") (car r) (caddr r)))
     (define all-pass (andmap cadr results))
     (printf "~n~a~n" (if all-pass "All gates passed. ✓" "Some gates failed. ✗"))])

  (exit (if (andmap cadr results) 0 1)))

(module+ main
  (main))
