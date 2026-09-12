#lang racket

;; @speed fast
;; @suite ci
;; @boundary integration
;; tests/test-w9-ci-workflow-verification.rkt
;; v0.99.40 W9 (#8526): PR CI and controlled release workflow verification
;;
;; This wave verifies that the remediation works in GitHub, not just locally:
;; 1. CI workflows reference setup-racket composite action
;; 2. release-repair.yml is properly configured for dry-run safety
;; 3. CI workflow structure has required jobs
;; 4. Release workflow has proper gate structure
;; 5. Evidence structure is valid for audit recording

(require rackunit
         racket/string
         racket/match
         racket/file
         racket/list
         racket/dict
         racket/system
         json
         racket/runtime-path
         (only-in "../util/version.rkt" q-version))

;; BUG-0009: version-bearing artifact expectations must derive from q-version
;; so a version bump fails loudly here instead of silently pinning stale literals.
(define topology-tag "v1.00.26") ;; frozen: the w2 topology checkpoint is a v1.00.26-series artifact

;; The declared verification invocation is `racket tests/<file>.rkt` from the
;; repo root, while `raco test` chdirs to this file's directory. Anchor on
;; this file's location so both styles (and the `../`-relative reads below)
;; resolve identically.
(define-runtime-path test-file-dir ".")
(current-directory (simplify-path test-file-dir))

;; W1: evaluate live script definitions so the lint-split pin follows
;; the real sources of truth (ci.yml + policy) rather than copies.
(define (dynamic-script sym)
  (dynamic-require (build-path ".." "scripts" "gsd-gates" "gate-claims.rkt") sym))

;; ---------------------------------------------------------------------------
;; Pure validation logic (no I/O)
;; ---------------------------------------------------------------------------

;; Validate a CI job name is present in the expected list.
(define (validate-ci-job-present job-name expected-jobs)
  (if (member job-name expected-jobs)
      (list 'found job-name)
      (list 'missing job-name)))

;; Validate all required CI jobs are present.
(define (validate-required-ci-jobs actual-jobs required-jobs)
  (define results
    (for/list ([job (in-list required-jobs)])
      (validate-ci-job-present job actual-jobs)))
  (define missing (filter (lambda (r) (eq? (car r) 'missing)) results))
  (if (null? missing)
      (list 'all-present)
      (list 'incomplete (map cadr missing))))

;; Validate setup-racket is referenced in workflow content.
(define (validate-setup-racket-ref workflow-content)
  (if (string-contains? workflow-content "setup-racket")
      (list 'referenced)
      (list 'not-referenced)))

;; Validate release-repair dry-run safety defaults.
(define (validate-repair-default-mode workflow-content)
  ;; Check that default is dry-run
  (if (and (string-contains? workflow-content "default: dry-run")
           (string-contains? workflow-content "mode"))
      (list 'safe-default)
      (list 'unsafe-default)))

;; Validate that a mode choice exists with the three expected options.
(define (validate-repair-mode-options workflow-content)
  (define has-dry-run (string-contains? workflow-content "dry-run"))
  (define has-publish (string-contains? workflow-content "publish"))
  (define has-repair-assets (string-contains? workflow-content "repair-assets"))
  (if (and has-dry-run has-publish has-repair-assets)
      (list 'all-modes-present)
      (list 'missing-modes
            (filter values
                    (list (and (not has-dry-run) "dry-run")
                          (and (not has-publish) "publish")
                          (and (not has-repair-assets) "repair-assets"))))))

;; Validate release.yml uses tag-publish context.
(define (validate-release-context workflow-content)
  (if (string-contains? workflow-content "tag-publish")
      (list 'correct-context)
      (list 'wrong-context)))

;; Validate release asset verification step exists.
(define (validate-asset-verification workflow-content)
  (if (or (string-contains? workflow-content "Verify") (string-contains? workflow-content "asset"))
      (list 'has-verification)
      (list 'no-verification)))

;; Build evidence record structure.
(define (build-evidence-record pr-number ci-run-id repair-run-id setup-racket-ok lint-ok)
  (hash 'pr-number
        pr-number
        'ci-run-id
        ci-run-id
        'repair-run-id
        repair-run-id
        'setup-racket-success
        setup-racket-ok
        'lint-all-pass
        lint-ok
        'timestamp
        (current-seconds)))

;; Validate evidence record has all required fields.
(define (validate-evidence-record record)
  (define required-keys '(pr-number ci-run-id repair-run-id setup-racket-success lint-all-pass))
  (define missing (filter (lambda (k) (not (hash-has-key? record k))) required-keys))
  (if (null? missing)
      (list 'complete)
      (list 'incomplete missing)))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(define-test-suite
 w9-ci-verification
 ;; ── CI workflow structure tests ──
 (test-case "ci.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/ci.yml"))
   (define content (file->string "../.github/workflows/ci.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 (test-case "release.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/release.yml"))
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 (test-case "release-repair.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/release-repair.yml"))
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 ;; ── CI job structure tests ──
 (test-case "ci.yml has required CI jobs"
   (define content (file->string "../.github/workflows/ci.yml"))
   (define required-jobs '("lint:" "test:" "security:" "smoke:" "workflows:" "release-readiness:"))
   (for ([job (in-list required-jobs)])
     (check-true (string-contains? content job) (format "ci.yml should contain job ~a" job))))
 (test-case "required CI job validation logic — all present"
   (define result (validate-required-ci-jobs '("lint" "test" "smoke") '("lint" "test")))
   (check-equal? result '(all-present)))
 (test-case "required CI job validation logic — incomplete"
   (define result (validate-required-ci-jobs '("lint") '("lint" "test" "smoke")))
   (check-equal? result '(incomplete ("test" "smoke"))))
 ;; ── release-repair.yml safety tests ──
 (test-case "release-repair.yml defaults to dry-run"
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-equal? (validate-repair-default-mode content) '(safe-default)))
 (test-case "release-repair.yml mode options — only dry-run"
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-true (string-contains? content "dry-run") "must contain dry-run")
   ;; Must NOT have publish or repair-assets mode options
   (check-false (string-contains? content "- publish") "must NOT have publish mode option")
   (check-false (string-contains? content "- repair-assets")
                "must NOT have repair-assets mode option"))
 ;; ── release.yml correctness tests ──
 (test-case "release.yml uses tag-publish context"
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-release-context content) '(correct-context)))
 (test-case "release.yml has asset verification"
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-asset-verification content) '(has-verification)))
 ;; ── Evidence record structure tests ──
 (test-case "evidence record — complete"
   (define record (build-evidence-record 8537 5610 5611 #t #t))
   (check-equal? (validate-evidence-record record) '(complete)))
 (test-case "evidence record — incomplete (missing ci-run-id)"
   (define record (make-hash))
   (hash-set! record 'pr-number 100)
   (hash-set! record 'repair-run-id 200)
   (hash-set! record 'setup-racket-success #t)
   (hash-set! record 'lint-all-pass #t)
   (check-equal? (validate-evidence-record record) '(incomplete (ci-run-id))))
 (test-case "evidence record contains timestamp"
   (define record (build-evidence-record 1 2 3 #t #t))
   (check-true (hash-has-key? record 'timestamp)))
 ;; ── CI dependency verification ──
 (test-case "setup-racket composite action exists"
   (check-true (file-exists? (build-path ".." ".github" "actions" "setup-racket" "action.yml"))))
 (test-case "ci-package-setup.rkt exists from v0.99.39"
   (check-true (file-exists? (build-path ".." "scripts" "ci-package-setup.rkt"))))
 (test-case "release-repair.rkt script exists from W7"
   (check-true (file-exists? (build-path ".." "scripts" "release-repair.rkt"))))
 (test-case "release-dry-run.rkt script exists from W3"
   (check-true (file-exists? (build-path ".." "scripts" "release-dry-run.rkt"))))
 ;; ── W0: required-check graph characterization (integration layer) ──
 ;; Pins, as literal data compiled into this module, the same graph that
 ;; tests/test-ci-runtime-contract.rkt pins against the live policy file and
 ;; the checksummed snapshot: the protected-main gate context list, the
 ;; aggregate needs edges, and fail-closed gate verdict semantics.
 (test-case "w0: required-check graph characterization"
   (define W0-REQUIRED-CHECKS
     '("lint" "lint-quality"
              "security"
              "release-dry-run"
              "smoke (ubuntu-latest)"
              "test (0)"
              "test (1)"
              "test (2)"
              "test-aggregate"
              "test-platform"
              "workflows (0)"
              "workflows (1)"
              "workflows-aggregate"))
   (define W0-AGGREGATE-NEEDS
     '(("test-aggregate" ("test (0)" "test (1)" "test (2)" "test-platform"))
       ("workflows-aggregate" ("workflows (0)" "workflows (1)"))))
   (define (w0-verdict required observed)
     (define missing (filter (lambda (n) (not (member n observed))) required))
     (if (null? missing)
         'pass
         (list 'fail missing)))
   (test-case "gate requires the pinned thirteen contexts, and only those"
     (check-equal? (length W0-REQUIRED-CHECKS) 13)
     (check-true (andmap (lambda (n) (and (member n W0-REQUIRED-CHECKS) #t))
                         '("lint" "lint-quality"
                                  "security"
                                  "release-dry-run"
                                  "test-aggregate"
                                  "test-platform"
                                  "workflows-aggregate")))
     (check-false (member "fast-env" W0-REQUIRED-CHECKS))
     (check-false (member "prepared-env-report" W0-REQUIRED-CHECKS))
     (check-false (member "shard-plan-report" W0-REQUIRED-CHECKS))
     (check-false (member "release-readiness" W0-REQUIRED-CHECKS))
     (check-false (member "lint-alignment" W0-REQUIRED-CHECKS)))
   (test-case "every aggregate job needs its shard jobs"
     (for-each (lambda (entry)
                 (check-true (and (member (car entry) W0-REQUIRED-CHECKS) #t)
                             (format "aggregate ~a must itself be required" (car entry)))
                 (for-each (lambda (shard)
                             (check-true (and (member shard W0-REQUIRED-CHECKS) #t)
                                         (format "~a shard ~a must be required" (car entry) shard)))
                           (cadr entry)))
               W0-AGGREGATE-NEEDS))
   (test-case "missing any required check fails the gate (fail-closed)"
     (check-equal? (w0-verdict W0-REQUIRED-CHECKS W0-REQUIRED-CHECKS) 'pass)
     (for-each (lambda (dropped)
                 (check-equal? (w0-verdict W0-REQUIRED-CHECKS (remove dropped W0-REQUIRED-CHECKS))
                               (list 'fail (list dropped))
                               (format "gate must fail closed when ~a is missing" dropped)))
               W0-REQUIRED-CHECKS))))

;; ── W1: atomic lint split characterization (campaign topology) ───────────
;; Pins the §7 contract: `lint` is lightweight (governance controller
;; tests + workflow YAML validation only), `lint-quality` carries the
;; Racket lint suite, and both are required in the live policy file.
;; The workflow, policy, and this pin ship in the same commit so there
;; is never a fail-open or never-reporting window.
(test-case "w1: lint split — lightweight lint + required lint-quality"
  (define ci-yml (file->string "../.github/workflows/ci.yml"))
  (define policy-text (string-join (file->lines "../scripts/required-pr-checks.policy") "\n"))

  (define (job-section text job-name)
    (define lines (string-split text "\n"))
    (define header-rx (format "^  ~a:\\s*$" (regexp-quote job-name)))
    (define start-idx
      (for/first ([l (in-list lines)]
                  [i (in-naturals)]
                  #:when (regexp-match? header-rx l))
        i))
    (and start-idx
         (let loop ([i (add1 start-idx)]
                    [acc '()])
           (cond
             [(>= i (length lines)) (string-join (reverse acc) "\n")]
             [(and (non-empty-string? (string-trim (list-ref lines i)))
                   (regexp-match? #rx"^  [a-zA-Z][a-zA-Z0-9_-]*:\\s*$" (list-ref lines i)))
              (string-join (reverse acc) "\n")]
             [else (loop (add1 i) (cons (list-ref lines i) acc))]))))

  (define lint-section (job-section ci-yml "lint"))
  (define lint-quality-section (job-section ci-yml "lint-quality"))

  (test-case "both jobs exist in ci.yml"
    (check-not-false lint-section)
    (check-not-false lint-quality-section))

  (test-case "lint is lightweight: governance + YAML validation only"
    (check-true (string-contains? lint-section "Protected governance controller tests"))
    (check-true (string-contains? lint-section "Workflow YAML validation"))
    ;; no Racket lint work may remain on the PR critical path
    (check-false (string-contains? lint-section "scripts/lint-all.rkt"))
    (check-false (string-contains? lint-section "check-version-expectations"))
    (check-false (string-contains? lint-section "check-lint-alignment"))
    (check-false (string-contains? lint-section "classify-metadata"))
    (check-false (string-contains? lint-section "setup-racket")))

  (test-case "lint-quality carries the full Racket lint suite"
    (check-true (string-contains? lint-quality-section "scripts/lint-all.rkt"))
    (check-true (string-contains? lint-quality-section "check-version-expectations"))
    (check-true (string-contains? lint-quality-section "check-lint-alignment"))
    (check-true (string-contains? lint-quality-section "metadata-inventory (artifact)"))
    (check-true (string-contains? lint-quality-section "metadata-lint (blocking)")))

  (test-case "policy requires lint and lint-quality"
    (check-true (string-contains? policy-text "\"lint\""))
    (check-true (string-contains? policy-text "\"lint-quality\""))
    (check-false (string-contains? policy-text "\"lint-alignment\"")))

  (test-case "lint stays required throughout the split (fail-closed)"
    ;; the lightweight lint job must remain in the policy: removing it
    ;; would open a fail-open window during the §7.1 apply sequence
    (define required (dynamic-script 'ci-required-jobs))
    (check-true (and (member "lint" required) #t))
    (check-true (and (member "lint-quality" required) #t)))

  ;; ---- W2: fast-env starts after the lightweight lint,
  ;; concurrent with lint-quality. The integration layer re-pins the live
  ;; workflow text and the recorded, checksum-bound DAG checkpoint.
  (test-case "w2: fast-env needs edge — after lightweight lint, concurrent with lint-quality"
    (define ci-text (file->string "../.github/workflows/ci.yml"))
    (define cp-dir (format "artifacts/ci-topology/~a-w2" topology-tag))
    (define cp-path (build-path ".." cp-dir "dag-checkpoint.json"))
    (check-true (file-exists? cp-path) "the W2 dag-checkpoint.json must exist")
    (check-true (file-exists? (build-path ".." cp-dir "SHA256SUMS"))
                "the W2 SHA256SUMS manifest must exist")
    ;; the checkpoint artifact must be checksum-consistent (verify-lane contract)
    (check-equal?
     (parameterize ([current-directory ".."])
       (system*/exit-code (find-executable-path "sha256sum") "-c" (build-path cp-dir "SHA256SUMS")))
     0
     "the recorded DAG checkpoint must match its SHA256SUMS manifest")
    (define cp-text (file->string cp-path))
    (define cp (call-with-input-file cp-path read-json))
    (check-true (string-contains? cp-text (format "\"wave\": \"~a-w2\"" topology-tag)))
    (define (needs-line-of text job-name)
      (define lines (string-split text "\n"))
      (define start (index-of lines (string-append "  " job-name ":")))
      (and start
           (let loop ([rest (drop lines (add1 start))])
             (cond
               [(null? rest) #f]
               [(string-prefix? (car rest) "    needs:") (string-trim (car rest))]
               [(or (equal? (string-trim (car rest)) "")
                    (string-prefix? (car rest) "  #")
                    (string-prefix? (car rest) "    "))
                (loop (cdr rest))]
               [else #f]))))
    ;; live workflow text: fast-env has exactly one needs edge — the lightweight lint
    (check-equal? (needs-line-of ci-text "fast-env")
                  "needs: [lint]"
                  "fast-env must wait exactly once, for the lightweight lint")
    (check-equal? (needs-line-of ci-text "test")
                  "needs: [lint, fast-env]"
                  "test keeps its W0/W1-pinned requirements")
    (check-false (needs-line-of ci-text "lint-quality") "lint-quality stays parallel: no needs edge")
    ;; recorded DAG: same pin, plus the explicit absence of an ordering edge
    (define edges (hash-ref cp 'needs_edges))
    (check-equal? (map ~a (hash-ref edges 'fast-env)) '("lint"))
    (check-equal? (hash-ref edges 'test) '("lint" "fast-env"))
    (check-equal? (map ~a (hash-ref edges 'test-platform)) '("lint"))
    (check-equal? (hash-ref edges 'fast-env_to_lint-quality_ordering_edge)
                  "none"
                  "no ordering edge in either direction between fast-env and lint-quality")
    ;; the prepared-env producer and the guarded restore stay byte-identical
    (define contract (hash-ref cp 'fast_env_verification_contract))
    (check-equal? (hash-ref contract 'prepare_action_sha256)
                  "be8c614ce36555a0bcbe8671d12699255825109c9ff8c83a8c2a40650a4e5a2d"
                  "manifest/OS/Racket/lockfile verification steps must stay byte-identical")
    (check-equal?
     (hash-ref contract 'setup_action_sha256)
     "a8532d7cfa683fe3e9ed3426dfcf11dd43531acec166ee14f7e802e016062784"
     "the guarded restore action must stay byte-identical (re-stamped by v1.00.29 W6; purge and restore steps unchanged)")
    (check-true (string-contains? ci-text "needs.fast-env.result")
                "test shards must keep gating PREPARED_ENV on the fast-env result")
    ;; same-SHA timing shape (topology checkpoint, not a cohort)
    (define timing (hash-ref cp 'timing_checkpoint))
    (check-equal? (hash-ref timing 'label) "topology checkpoint — not a cohort")
    (check-equal? (hash-ref timing 'measurement_kind) "derived-shape-projection")
    (define (job-time side name)
      (for/first ([j (in-list (hash-ref side 'jobs))]
                  #:when (equal? (hash-ref j 'name) name))
        j))
    (define lint-after (job-time (hash-ref timing 'after) "lint"))
    (define lq-after (job-time (hash-ref timing 'after) "lint-quality"))
    (define fe-after (job-time (hash-ref timing 'after) "fast-env"))
    (define fe-before (job-time (hash-ref timing 'before) "fast-env"))
    (check-not-false (and lint-after lq-after fe-after fe-before))
    (check-true (string<? (hash-ref lint-after 'completed_at) (hash-ref fe-after 'started_at))
                "fast-env starts only after the lightweight lint completes")
    (check-true (string<? (hash-ref fe-after 'started_at) (hash-ref fe-before 'started_at))
                "fast-env starts earlier under the W2 edge (same-SHA shape)")
    (check-true (string<? (hash-ref lq-after 'started_at) (hash-ref fe-after 'completed_at))
                "lint-quality is still running when fast-env starts (concurrent)")
    (check-true (string<? (hash-ref fe-after 'started_at) (hash-ref lq-after 'completed_at))
                "fast-env is still running when lint-quality finishes (concurrent)")))

(module+ test
  (require rackunit/text-ui)
  (run-tests w9-ci-verification))

(module+ main
  (require rackunit/text-ui)
  (exit (run-tests w9-ci-verification)))
