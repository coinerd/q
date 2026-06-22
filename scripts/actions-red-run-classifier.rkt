#!/usr/bin/env racket
#lang racket/base

;; scripts/actions-red-run-classifier.rkt — Actions red-run classifier.
;;
;; W7 (#8547): Classify historical vs current blocking red runs.
;;
;; The classifier takes structured GitHub Actions run data and produces
;; one of 7 verdicts:
;;
;;   current_blocking_red_release_run  — latest release.yml run is red
;;   current_blocking_red_main_ci      — latest ci.yml run on main is red
;;   historical_superseded_red         — older run that a later run superseded
;;   cancelled_superseded              — cancelled run superseded by later success
;;   in_progress_blocking              — a run is still in progress
;;   success_current                   — latest runs are all green
;;   unknown_due_api_error             — API query failed; blocks until retried
;;
;; Usage:
;;   cd q/ && racket scripts/actions-red-run-classifier.rkt <milestone-number> [--json]

(provide classify-red-run
         classify-red-run-set
         make-red-run-result
         all-blocked-verdicts
         blocking-verdict?
         superseded-verdict?
         success-verdict?
         RED_RUN_VERDICTS)

(require racket/file
         racket/list
         racket/port
         racket/string
         racket/system
         json)

;; ---------------------------------------------------------------------------
;; Pure logic (testable without network)
;; ---------------------------------------------------------------------------

;; All possible verdicts as strings.
(define RED_RUN_VERDICTS
  '("current_blocking_red_release_run" "current_blocking_red_main_ci"
                                       "historical_superseded_red"
                                       "cancelled_superseded"
                                       "in_progress_blocking"
                                       "success_current"
                                       "unknown_due_api_error"))

;; DESIGN FACT (W9, v0.99.42 §44): `cancelled_superseded` is intentionally
;; excluded from all-blocked-verdicts. A cancelled run is superseded — it
;; does not block milestone approval because the workflow can be re-run.
;;
;; However, classify-red-run-set sets `'blocking #t` in the result hash for
;; cancelled latest runs. This is intentional: the hash field signals
;; "human attention needed" while blocking-verdict? controls the gate.
;;
;; DO NOT add `cancelled_superseded` to all-blocked-verdicts without
;; also updating the gate logic — it would make cancelled runs
;; unconditionally block milestones, which is wrong.
(define all-blocked-verdicts
  '("current_blocking_red_release_run" "current_blocking_red_main_ci"
                                       "in_progress_blocking"
                                       "unknown_due_api_error"))

;; Check if a verdict string blocks approval.
(define (blocking-verdict? verdict)
  (and verdict (member verdict all-blocked-verdicts) #t))

;; W2 (#8564): Verdicts that indicate a superseded (historical) red run.
(define superseded-verdicts '("historical_superseded_red" "cancelled_superseded"))

;; Check if a verdict is historical/superseded (does not block approval).
(define (superseded-verdict? verdict)
  (and verdict (member verdict superseded-verdicts) #t))

;; W2 (#8564): Check if a verdict indicates the latest run is green.
(define (success-verdict? verdict)
  (and verdict (equal? verdict "success_current") #t))

;; Build a structured red-run result hash for JSON output.
(define (make-red-run-result verdict run-type run-id run-number conclusion detail blocking?)
  (hasheq 'verdict
          verdict
          'run_type
          run-type
          'run_id
          (or run-id 0)
          'run_number
          (or run-number 0)
          'conclusion
          (or conclusion "unknown")
          'detail
          (or detail "")
          'blocking
          blocking?))

;; Classify a single GitHub Actions run for red-run purposes.
;;
;; Inputs:
;;   run-data: hash or #f
;;     'conclusion → "success" | "failure" | "cancelled" | #f (in-progress)
;;     'status → "completed" | "in_progress" | "queued" | #f
;;     'run_number → integer
;;     'id → integer
;;   run-type: string — "release" or "ci" or "ci-main"
;;   is-latest?: boolean — is this the most recent run for this workflow+branch?
;;   api-error?: boolean — did the API call fail?
;;
;; Returns a verdict string.
(define (classify-red-run run-data run-type is-latest? api-error?)
  (cond
    ;; API error — unknown, blocks approval
    [api-error? "unknown_due_api_error"]
    ;; No run data at all — treat as unknown
    [(not run-data) "unknown_due_api_error"]
    ;; Run still in progress — blocks
    [(let ([status (hash-ref run-data 'status #f)]) (and status (not (equal? status "completed"))))
     "in_progress_blocking"]
    ;; Run completed and succeeded
    [(equal? (hash-ref run-data 'conclusion #f) "success") "success_current"]
    ;; Run was cancelled
    [(equal? (hash-ref run-data 'conclusion #f) "cancelled")
     (if is-latest? "cancelled_superseded" "cancelled_superseded")]
    ;; Run failed — determine if current/latest or historical
    [(equal? (hash-ref run-data 'conclusion #f) "failure")
     (if is-latest?
         (if (equal? run-type "release")
             "current_blocking_red_release_run"
             "current_blocking_red_main_ci")
         "historical_superseded_red")]
    ;; Any other conclusion
    [else "unknown_due_api_error"]))

;; Classify a set of runs (e.g., all release runs or all CI runs on main).
;;
;; This function takes a list of runs (newest first) and classifies the
;; overall red-run state. If the latest run is green, all earlier red
;; runs are "historical_superseded_red". If the latest run is red, it's
;; "current_blocking".
;;
;; Inputs:
;;   runs: list of run-data hashes (newest first), or '() or #f
;;   run-type: "release" or "ci" or "ci-main"
;;   api-error?: boolean
;;
;; Returns a red-run result hash.
(define (classify-red-run-set runs run-type api-error?)
  (cond
    [api-error?
     (make-red-run-result "unknown_due_api_error"
                          run-type
                          #f
                          #f
                          "unknown"
                          "API query failed — blocks until retried"
                          #t)]
    [(or (not runs) (null? runs))
     (make-red-run-result "unknown_due_api_error"
                          run-type
                          #f
                          #f
                          "none"
                          (format "No ~a runs found" run-type)
                          #t)]
    [else
     (define latest (car runs))
     (define status (hash-ref latest 'status #f))
     (define conclusion (hash-ref latest 'conclusion #f))
     (define run-id (hash-ref latest 'id 0))
     (define run-number (hash-ref latest 'run_number 0))
     (cond
       ;; Still in progress
       [(and status (not (equal? status "completed")))
        (make-red-run-result "in_progress_blocking"
                             run-type
                             run-id
                             run-number
                             conclusion
                             (format "~a run ~a is ~a — blocks approval" run-type run-number status)
                             #t)]
       ;; Latest succeeded
       [(equal? conclusion "success")
        (make-red-run-result "success_current"
                             run-type
                             run-id
                             run-number
                             conclusion
                             (format "~a run ~a: success (latest green)" run-type run-number)
                             #f)]
       ;; Latest cancelled
       [(equal? conclusion "cancelled")
        (make-red-run-result "cancelled_superseded"
                             run-type
                             run-id
                             run-number
                             conclusion
                             (format "~a run ~a: cancelled (latest)" run-type run-number)
                             #t)]
       ;; Latest failed — current blocking
       [(equal? conclusion "failure")
        (define verdict
          (if (equal? run-type "release")
              "current_blocking_red_release_run"
              "current_blocking_red_main_ci"))
        (make-red-run-result
         verdict
         run-type
         run-id
         run-number
         conclusion
         (format "~a run ~a: FAILURE (latest) — blocks approval" run-type run-number)
         #t)]
       [else
        (make-red-run-result
         "unknown_due_api_error"
         run-type
         run-id
         run-number
         conclusion
         (format "~a run ~a: unexpected conclusion '~a'" run-type run-number conclusion)
         #t)])]))

;; ---------------------------------------------------------------------------
;; CLI (network-dependent)
;; ---------------------------------------------------------------------------

(define (gh-api-json path)
  (define token
    (with-handlers ([exn:fail? (λ (_) "")])
      (string-trim (file->string (build-path (find-system-path 'home-dir) "GH_PAT")))))
  (define cmd
    (format
     "curl -s -H \"Authorization: token ~a\" -H \"Accept: application/vnd.github+json\" \"https://api.github.com/repos/coinerd/q/~a\""
     token
     path))
  (define output (with-output-to-string (λ () (system cmd))))
  (with-handlers ([exn:fail? (λ (_) #f)])
    (string->jsexpr output)))

(define (extract-version-from-milestone-title title)
  (define m (regexp-match #rx"v?([0-9]+\\.[0-9]+\\.[0-9]+)" title))
  (and m (cadr m)))

(define args (vector->list (current-command-line-arguments)))
(define json-output? (member "--json" args))

(define milestone-number
  (for/first ([a (in-list args)]
              #:when (regexp-match? #rx"^[0-9]+$" a))
    (string->number a)))

(define (main)
  (unless milestone-number
    (printf "Usage: racket scripts/actions-red-run-classifier.rkt <milestone-number> [--json]~n")
    (exit 0))

  ;; Get milestone info
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (unless ms-data
    (printf "ERROR: Could not query milestone #~a~n" milestone-number)
    (exit 1))

  (define title (hash-ref ms-data 'title ""))
  (define version (extract-version-from-milestone-title title))

  (unless version
    (printf "ERROR: Cannot extract version from milestone title: ~a~n" title)
    (exit 1))

  (define tag (format "v~a" version))

  ;; Query release workflow runs for this tag
  (define rel-wf-data
    (gh-api-json (format "actions/workflows/release.yml/runs?branch=~a&per_page=10" tag)))
  (define release-runs
    (if (and rel-wf-data (hash? rel-wf-data))
        (hash-ref rel-wf-data 'workflow_runs '())
        '()))
  (define release-api-error? (not rel-wf-data))

  ;; Query latest CI runs on main
  (define ci-wf-data (gh-api-json "actions/runs?branch=main&per_page=5"))
  (define ci-runs
    (if (and ci-wf-data (hash? ci-wf-data))
        (hash-ref ci-wf-data 'workflow_runs '())
        '()))
  (define ci-api-error? (not ci-wf-data))

  ;; Classify
  (define release-result (classify-red-run-set release-runs "release" release-api-error?))
  (define ci-result (classify-red-run-set ci-runs "ci-main" ci-api-error?))

  ;; Overall verdict — worst of the two
  (define release-blocking? (hash-ref release-result 'blocking #t))
  (define ci-blocking? (hash-ref ci-result 'blocking #t))
  (define overall-blocking? (or release-blocking? ci-blocking?))

  (define overall-result
    (hasheq 'milestone
            milestone-number
            'version
            version
            'tag
            tag
            'release_classification
            release-result
            'ci_classification
            ci-result
            'overall_blocking
            overall-blocking?))

  (cond
    [json-output? (printf "~a~n" (jsexpr->string overall-result))]
    [else
     (printf "=== Red-Run Classification for ~a (#~a) ===~n~n" tag milestone-number)
     (printf "Release workflow:~n")
     (printf "  Verdict:    ~a~n" (hash-ref release-result 'verdict))
     (printf "  Run #:      ~a~n" (hash-ref release-result 'run_number))
     (printf "  Conclusion: ~a~n" (hash-ref release-result 'conclusion))
     (printf "  Blocking:   ~a~n" (hash-ref release-result 'blocking))
     (printf "  Detail:     ~a~n~n" (hash-ref release-result 'detail))
     (printf "Main CI:~n")
     (printf "  Verdict:    ~a~n" (hash-ref ci-result 'verdict))
     (printf "  Run #:      ~a~n" (hash-ref ci-result 'run_number))
     (printf "  Conclusion: ~a~n" (hash-ref ci-result 'conclusion))
     (printf "  Blocking:   ~a~n" (hash-ref ci-result 'blocking))
     (printf "  Detail:     ~a~n~n" (hash-ref ci-result 'detail))
     (printf "Overall blocking: ~a~n" overall-blocking?)
     (if overall-blocking?
         (printf "❌ RED-RUN CLASSIFICATION: BLOCKS APPROVAL~n")
         (printf "✅ RED-RUN CLASSIFICATION: NO BLOCKING RED RUNS~n"))])

  (exit (if overall-blocking? 1 0)))

(module+ main
  (main))
