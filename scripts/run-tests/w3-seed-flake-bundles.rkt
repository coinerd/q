#lang racket/base

;; q/scripts/run-tests/w3-seed-flake-bundles.rkt — v1.00.29 W3 one-off
;;
;; Regenerates the two SEED incident bundles under
;; artifacts/proof-graph/v1.00.29-w3/bundles/ from the v1.00.28 W9 live
;; observations:
;;   - incident A: tests/test-ci-cohort-report.rkt failed in full-suite
;;     context, standalone green (later retried green — the rerun is recorded
;;     as a separate observation, the original failure is retained);
;;   - incident B: tests/test-milestone-gate.rkt rotating failure, same shape.
;;
;; Both are post-hoc captures: every field that was not recorded at failure
;; time is the string "unknown" (never omitted), and the root-cause class is
;; deliberately "unknown" — no cause is predeclared (BUG-0065/BUG-0066 are
;; admissible hypotheses, see docs/reports/FLAKE-FORENSICS-v1.00.29.md).
;;
;; The bundles are built with the SAME canonicalization + incident-id chain
;; as live captures (flake-forensics.rkt), so they conform to
;; q.flake-forensics/1 by construction. A SHA256SUMS file is regenerated for
;; the whole bundles directory.
;;
;; raco test runs every .rkt in this directory at module load; keep this
;; one-off's side effects behind the direct-invocation guard (W0: raco test
;; must pass) — same pattern as w3-exit-scan.rkt.

(require racket/file
         racket/list
         racket/path
         racket/string
         (only-in "flake-forensics.rkt" flake-forensics-schema incident-id write-json-bundle!)
         (only-in "sha256.rkt" sha256-hex))

(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "w3-seed-flake-bundles.rkt"))))))

(define (seed-bundle #:failing-test-file file
                     #:observed-on date
                     #:context-note note
                     #:rerun-note rerun-note)
  ;; Every unrecorded field is "unknown"; the two recorded facts are the
  ;; failing file and the suite-context-only failure shape itself.
  (hasheq 'schema
          flake-forensics-schema
          'source-note
          "v1.00.28 W9 live observation, post-hoc capture"
          'root-cause-class
          "unknown"
          'git
          (hasheq 'commit "unknown" 'tree "unknown")
          'workflow
          (hasheq 'workflow-id "unknown" 'run-id "unknown" 'job-id "unknown")
          'failing-test-file
          file
          'behavior-ids
          "unknown"
          'predecessor-sequence
          "unknown"
          'shard
          "unknown"
          'worker
          "unknown"
          'scheduler-mode
          "unknown"
          'seed
          "unknown"
          'selected-manifest-digest
          "unknown"
          'compiled-state
          (hasheq 'paths-digest "unknown" 'state "unknown")
          'prepared-env
          "unknown"
          'environment-policy-digest
          "unknown"
          'temp-roots
          (hasheq 'tmpdir "unknown" 'temp "unknown")
          'filesystem-residue
          "unknown"
          'child-process-tree
          "unknown"
          'surviving-pids
          "unknown"
          'open-handles
          "unknown"
          'worktrees
          "unknown"
          'captured-at
          (string-append date "T00:00:00Z")
          'captured-at-precision
          "date-only, post-hoc reconstruction (exact wall time not recorded)"
          'test-durations
          "unknown"
          'original-stdout
          "unknown"
          'original-stderr
          "unknown"
          'original-result-artifact
          "unknown"
          'rerun-ancestry
          '()
          'context-note
          note
          'rerun-observations
          (list (hasheq 'observation-kind
                        "rerun"
                        'result
                        "green"
                        'recorded-as
                        "separate observation; the original failure is retained in this bundle"
                        'observed-on
                        (string-append date " (date-only, post-hoc)")
                        'pattern
                        rerun-note))))

(define (main)
  (define root
    (simplify-path (build-path (let-values ([(base _name _dir?)
                                             (split-path (resolved-module-path-name
                                                          (variable-reference->resolved-module-path
                                                           (#%variable-reference))))])
                                 base)
                               'up
                               'up)))
  (define bundles-dir (build-path root "artifacts" "proof-graph" "v1.00.29-w3" "bundles"))
  (define incident-a
    (seed-bundle
     #:failing-test-file "tests/test-ci-cohort-report.rkt"
     #:observed-on "2026-09-08"
     #:context-note
     (string-append "failed in the full-suite worker context while the standalone run was "
                    "green; the failure did not reproduce on retry")
     #:rerun-note "single green retry observed; no rotation recorded"))
  (define incident-b
    (seed-bundle
     #:failing-test-file "tests/test-milestone-gate.rkt"
     #:observed-on "2026-09-09"
     #:context-note
     (string-append "rotating failure in the full-suite worker context while the standalone "
                    "run was green; same failure shape as incident A")
     #:rerun-note "rotating: green on some suite runs, red on others; standalone always green"))
  (for ([bundle (in-list (list incident-a incident-b))])
    (define full (hash-set bundle 'incident-id (incident-id bundle)))
    (define path (build-path bundles-dir (string-append (hash-ref full 'incident-id) ".json")))
    (write-json-bundle! path full)
    (printf "wrote ~a~n" path))
  ;; Regenerate SHA256SUMS over the whole bundles directory.
  (define sums
    (for/list ([f (in-list (sort (directory-list bundles-dir) path<?))]
               #:when (string-suffix? (path->string f) ".json"))
      (format "~a  ~a" (sha256-hex (file->bytes (build-path bundles-dir f))) (path->string f))))
  (define sums-path (build-path bundles-dir "SHA256SUMS"))
  (with-output-to-file sums-path
                       (lambda ()
                         (for ([line (in-list sums)])
                           (displayln line)))
                       #:exists 'replace)
  (printf "wrote ~a (~a entries)~n" sums-path (length sums)))

(when invoked-directly?
  (main))
