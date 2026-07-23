#lang racket/base

;; q/scripts/run-tests/classify-filters.rkt — Suite classification predicates
;;
;; Extracted from classify.rkt in v0.99.58 W3-1 (P3-CL).
;; Pure predicates that classify test files into suites (slow, tui,
;; mutating, security, smoke, platform, arch, runtime, extensions, etc.).
;; Depends on classify-metadata for get-file-metadata and base-dir.
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/path
         racket/file
         "classify-metadata.rkt")

(provide slow-patterns
         path-slow-patterns
         mutating-patterns
         support-test-module-names
         smoke-curated-files
         release-smoke-curated-files
         platform-curated-files
         slow-file?
         tui-file?
         mutating-file?
         security-file?
         file-has-suite-tag?
         smoke-excluded?
         smoke-included?
         release-smoke-included?
         support-test-module?
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         unit-fast-file?
         file-has-rackunit-tests?
         platform-file?)

;; ============================================================
;; Patterns
;; ============================================================

(define slow-patterns
  '("sandbox" "subprocess"
              "integration"
              "benchmark"
              "workflow-"
              "e2e-"
              "tmux-explore-campaign"
              "tmux-explore-multi-step"
              "tmux-explore-evidence"
              "tmux-explore-executor"
              "tmux-explore-production-provenance"
              "tmux-explore-verifiers"
              "tmux-q-harness"
              "tmux-tui-"
              "ci-local"
              "metrics-readme"
              "examples-compile"
              "pre-commit"
              "racket-tooling"
              "run-tests"
              "audit-script"
              "test-doctor"
              "check-deps"
              "self-hosting"
              "tui-terminal"
              "sync-readme"))

(define path-slow-patterns '("/workflows/"))

(define mutating-patterns
  '("ci-local" "pre-commit"
               "check-deps"
               "sync-version"
               "sync-readme"
               "bump-version"
               "metrics-readme"
               "self-hosting"))

(define support-test-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

;; ============================================================
;; Classifiers
;; ============================================================

(define (slow-file? f)
  (define meta (get-file-metadata f))
  (define meta-speed (hash-ref meta 'speed #f))
  (cond
    [(eq? meta-speed 'fast) #f]
    [(eq? meta-speed 'slow) #t]
    [(eq? meta-speed 'perf) #t]
    [else
     (define base (file-name-from-path f))
     (or (for/or ([p (in-list slow-patterns)])
           (and base (string-contains? (path->string base) p)))
         (for/or ([p (in-list path-slow-patterns)])
           (string-contains? f p)))]))

(define (tui-file? f)
  (define meta (get-file-metadata f))
  (define meta-suite (hash-ref meta 'suite #f))
  (if (equal? meta-suite "tui")
      #t
      (let ([base (file-name-from-path f)])
        (or (string-contains? f "/tui/")
            (string-contains? f "/interfaces/tui.rkt")
            (and base (string-prefix? (path->string base) "test-tui-"))))))

(define (mutating-file? f)
  (define meta (get-file-metadata f))
  (define meta-mutates (hash-ref meta 'mutates #f))
  (define meta-isolation (hash-ref meta 'isolation #f))
  (cond
    [(equal? meta-mutates "none") #f]
    [(equal? meta-isolation "process") #t]
    [else
     (let ([base (file-name-from-path f)])
       (for/or ([p (in-list mutating-patterns)])
         (and base (string-contains? (path->string base) p))))]))

(define (security-file? f)
  (define base (path->string (file-name-from-path f)))
  (or (string-contains? base "security")
      (string-contains? base "permission")
      (string-contains? base "safe-mode")
      (string-contains? base "sandbox")
      (string-contains? base "tool-bash")
      (file-has-suite-tag? f "security")))

(define (file-has-suite-tag? f tag)
  (define meta (get-file-metadata f))
  (define suites (hash-ref meta 'suites '()))
  (or (member tag suites) #f))

(define (smoke-excluded? f)
  (or (slow-file? f) (string-contains? f "/workflows/") (string-contains? f "/interfaces/")))

;; Smoke suite: curated sanity-gate of ~20 fast, always-green tests.
;; Verifies the system can boot, import core modules, and perform basic operations.
;; Files are selected by @suite smoke tag OR by matching the curated list below.
(define smoke-curated-files
  '("tests/test-version.rkt" "tests/test-safe-mode.rkt"
                             "tests/test-error-classify.rkt"
                             "tests/test-mutating-tool-taxonomy.rkt"
                             "tests/test-run-tests-script.rkt"
                             "tests/test-worker-security.rkt"
                             "tests/test-spawn-subagent-serialization.rkt"
                             "tests/test-verifier-gate.rkt"
                             "tests/test-cli-flags.rkt"
                             "tests/test-extension-tiers.rkt"
                             "tests/test-capability-aware-spawn.rkt"
                             "tests/test-frontmatter-extended.rkt"
                             "tests/test-context-assembly-config.rkt"
                             "tests/test-execution-plane-error-label.rkt"
                             "tests/test-runtime-packages.rkt"
                             "tests/test-cli.rkt"
                             "tests/test-tui-hotspot-characterization.rkt"
                             "tests/test-tui-render-loop.rkt"
                             "tests/test-tui-frame-integrity.rkt"
                             "tests/test-tui-goal-status-bar.rkt"))

(define (smoke-included? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "smoke")
      (for/or ([curated (in-list smoke-curated-files)])
        (string-suffix? s curated))))

;; Release-smoke suite: post-release artifact verification.
;; Deterministic subset of smoke that focuses on verifying the installed
;; artifact works correctly. No browser/playwright, no live network,
;; no terminal, no mutating tests.
;; Contract: tests that verify version, CLI, core system boot, and
;; selected release-critical functionality only.
(define release-smoke-curated-files
  '("tests/test-version.rkt" "tests/test-safe-mode.rkt"
                             "tests/test-error-classify.rkt"
                             "tests/test-mutating-tool-taxonomy.rkt"
                             "tests/test-verifier-gate.rkt"
                             "tests/test-cli-flags.rkt"
                             "tests/test-extension-tiers.rkt"
                             "tests/test-capability-aware-spawn.rkt"
                             "tests/test-frontmatter-extended.rkt"
                             "tests/test-context-assembly-config.rkt"
                             "tests/test-execution-plane-error-label.rkt"
                             "tests/test-runtime-packages.rkt"
                             "tests/test-spawn-subagent-serialization.rkt"))

(define (release-smoke-included? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "release-smoke")
      (for/or ([curated (in-list release-smoke-curated-files)])
        (string-suffix? s curated))))

(define (support-test-module? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (define base (file-name-from-path s))
  (or (string-contains? s "/helpers/")
      (string-contains? s "/fixtures/")
      (and base (member (path->string base) support-test-module-names) #t)))

(define (arch-file? f)
  (or (string-contains? f "arch-")
      (string-contains? f "boundary")
      (string-contains? f "fitness")
      (string-contains? f "hotspot")
      (file-has-suite-tag? f "arch")))

(define (runtime-file? f)
  (or (string-contains? f "runtime")
      (string-contains? f "session")
      (string-contains? f "compaction")
      (string-contains? f "iteration")
      (string-contains? f "turn-")
      (string-contains? f "tool-coord")
      (file-has-suite-tag? f "runtime")))

(define (extensions-file? f)
  (or (string-contains? f "extensions/")
      (string-contains? f "gsd-")
      (string-contains? f "define-extension")
      (string-contains? f "wave-executor")
      (string-contains? f "hook-")
      (file-has-suite-tag? f "extensions")))

(define (workflows-file? f)
  (and (string-contains? f "/workflows/")
       (or (not (string-contains? f "/fixtures/"))
           (string-prefix? (path->string (file-name-from-path f)) "test-"))))

(define (unit-fast-file? f)
  (define meta (get-file-metadata f))
  (define speed (hash-ref meta 'speed #f))
  (define boundary (hash-ref meta 'boundary #f))
  (and (not (mutating-file? f))
       (or (file-has-suite-tag? f "unit-fast") (and (eq? speed 'fast) (equal? boundary "unit")))))

(define (file-has-rackunit-tests? path)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [has-rackunit-forms? (or (regexp-match? #rx"\\(test-case" content)
                                       (regexp-match? #rx"\\(check-" content))]
              [module-plus-test? (regexp-match? #px"\\(module\\+\\s+test\\b" content)]
              [uses-rackunit-text-ui? (and has-rackunit-forms?
                                           (regexp-match? #rx"\\(run-tests" content))]
              [needs-raco-discovery?
               (and has-rackunit-forms? (not module-plus-test?) (not uses-rackunit-text-ui?))])
         (or module-plus-test? needs-raco-discovery?))))

;; ============================================================
;; Platform-cross classifier
;; Tests that exercise platform-specific paths: filesystem,
;; subprocess, terminal detection, path handling, Unicode.
;; These are the only tests that need macOS verification.
;; ============================================================

(define platform-curated-files
  '("tests/test-subprocess.rkt" "tests/test-subprocess-helpers.rkt"
                                "tests/test-subprocess-edge-cases.rkt"
                                "tests/test-cwd-independence.rkt"
                                "tests/test-config-paths.rkt"
                                "tests/test-sandbox.rkt"
                                "tests/test-worker-security.rkt"
                                "tests/test-tool-bash-workflow.rkt"
                                "tests/test-version.rkt"
                                "tests/test-safe-mode.rkt"
                                "tests/test-cli.rkt"
                                "tests/test-spawn-subagent-serialization.rkt"
                                "tests/test-runtime-packages.rkt"
                                "tests/test-extension-tiers.rkt"
                                "tests/test-capability-aware-spawn.rkt"
                                "tests/test-execution-plane-error-label.rkt"
                                "tests/test-error-classify.rkt"
                                "tests/test-mutating-tool-taxonomy.rkt"
                                "tests/test-verifier-gate.rkt"
                                "tests/test-cli-flags.rkt"
                                "tests/test-frontmatter-extended.rkt"
                                "tests/test-context-assembly-config.rkt"
                                "tests/test-metrics-helpers.rkt"
                                "tests/test-token-estimate-cache.rkt"
                                "tests/test-truncation.rkt"
                                "tests/test-lockfile.rkt"
                                "tests/test-facade-surface.rkt"
                                "tests/test-red-module-boundaries.rkt"
                                "tests/test-export-formats.rkt"
                                "tests/test-message-layout.rkt"
                                "tests/test-cell-diff-render.rkt"
                                "tests/test-llm-model.rkt"
                                "tests/test-llm-error-visibility.rkt"
                                "tests/test-streaming-transitions.rkt"
                                "tests/test-streaming-tool-bug.rkt"
                                "tests/test-loop-cancellation.rkt"
                                "tests/test-loop-edge-cases.rkt"
                                "tests/test-loop-events.rkt"
                                "tests/test-settings.rkt"
                                "tests/test-sync-readme-status.rkt"))

(define (platform-file? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "platform")
      (for/or ([curated (in-list platform-curated-files)])
        (string-suffix? s curated))))
