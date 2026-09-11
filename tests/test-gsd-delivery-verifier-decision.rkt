#lang racket/base
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;; @timeout 120  ;; per-owner cap: measured cold median ~3s (v1.00.29 W0 benchmark), 120s = default cap
;;
;; v1.00.29 W2 delivery-verifier boundary extraction — OWNER 1 of 3.
;;
;; This file is the unit-fast DECISION owner: verifier decision logic (branch
;; matching, changed-file detection, gate verdicts, verify normalization)
;; exercised over SYNTHETIC Git facts injected via current-gsd-git-runner.
;; Zero branches, commits, or working-tree churn: exactly one `git init`
;; per fixture so find-git-root-dir resolves the two-tier layout, and every
;; Git fact itself comes from the injected fake runner. Deterministic and
;; safely parallelizable; no real-Git boundary claim lives here.
;;
;; Ownership siblings (see docs/reports/DELIVERY-VERIFIER-SPLIT-v1.00.29.md):
;;   - tests/test-gsd-delivery-verifier-git-contract.rkt — required real-Git
;;     fail-closed boundary canaries (private fixture, integration lane)
;;   - tests/test-gsd-delivery-verifier-e2e.rkt — verify-gate execution plane
;;     + coordinator composition (e2e lane)
;;
;; Timing provenance: the stale whole-file `@timeout 300` / `~123s` note from
;; v1.00.28 (a2a10b9d) is superseded. This owner's cap is the 120s default,
;; derived from the v1.00.29 W0 benchmark contract (repeatable cold/warm
;; runs), not from the legacy observation. No global timeout increase.
;;
;; tests/test-gsd-delivery-verifier-decision.rkt — synthetic decision suite
;; for the /go delivery verifier (fail-closed structured delivery evidence).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         racket/match
         (only-in "helpers/delivery-fixtures.rkt"
                  GIT
                  write-plan!
                  write-wave-doc!
                  write-state!
                  load-plan*
                  load-plan**
                  cleanup-tmp)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message
                  current-gsd-delivery-verify-command
                  current-gsd-delivery-verify-timeout-sec
                  current-gsd-git-runner
                  normalize-declared-verify)
         (only-in "../extensions/gsd/events.rkt"
                  make-event-collector
                  set-gsd-event-bus!
                  gsd-event-bus-box)
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt" make-verification-registry)
         (only-in "helpers/private-fixture-templates.rkt" call-with-private-git-environment))

;; ============================================================
;; Synthetic Git facts (decision suite)
;; ============================================================

;; Minimal base-dir + q/ git-root scaffold for decision-logic tests: ONE
;; `git init` so find-git-root-dir resolves the two-tier layout; every Git
;; fact itself comes from the injected fake runner — no branches, no
;; commits, no working-tree churn, no git subprocess per assertion.
(define (make-synthetic-repo)
  (define base (make-temporary-file "dv-synth-~a" 'directory))
  (make-directory* (build-path base ".planning" "waves"))
  (make-directory* (build-path base "q" "ui-core"))
  (call-with-output-file (build-path base "q" "ui-core" "preferences.rkt")
                         (lambda (out)
                           (display "#lang racket/base\n(provide foo)\n(define foo 1)\n" out))
                         #:exists 'truncate)
  (parameterize ([current-directory (build-path base "q")])
    (unless (zero? (system*/exit-code GIT "init" "-q" "."))
      (error 'make-synthetic-repo "git init failed")))
  base)

;; Standard synthetic campaign fixture: plan + W0 wave doc + STATE.md issue
;; row over a synthetic repo. The declared verify is the deterministic
;; "exit 0" gate (the verify-gate decision is what this suite owns; the
;; real command-execution plane lives in the e2e owner).
(define (make-synthetic-campaign!)
  (define base (make-synthetic-repo))
  (write-plan! base 0 "Wave Zero" "zero")
  (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
  (write-state! base 0 "42")
  base)

;; Injectable run-git* replacement. Dispatches on the git subcommand the
;; verifier probes; every fact is synthetic. Changed/untracked paths are
;; GIT-ROOT-RELATIVE (what `git diff --name-only` prints when run from the
;; git root): a declared target "q/ui-core/preferences.rkt" maps to the
;; observed fact "ui-core/preferences.rkt". Unknown invocations fail
;; closed (exit 1) so a fake can never silently approve an unplanned
;; boundary read. Result shape matches run-git*: (list exit-code stdout
;; stderr).
(define (fake-git-facts #:branch [branch "feature/issue-42-wave"]
                        #:inside-work-tree? [inside? #t]
                        #:head-changed [head-changed '("ui-core/preferences.rkt")]
                        #:untracked [untracked '()]
                        #:committed [committed '("ui-core/preferences.rkt")]
                        #:base-ref [base-ref "main"])
  (lambda (git-root args)
    (match args
      [(list "rev-parse" "--is-inside-work-tree")
       (list (if inside? 0 128) (if inside? "true\n" "false\n") "")]
      [(list "rev-parse" "--abbrev-ref" "HEAD") (list 0 (string-append branch "\n") "")]
      [(list "rev-parse" "--verify" _) (list 0 (string-append base-ref "\n") "")]
      [(list "rev-parse" _ ...) (list 0 "deadbeef\n" "")]
      [(list "diff" "--name-only" "HEAD")
       (list 0 (string-append (string-join head-changed "\n") "\n") "")]
      [(list "diff" "--name-only" _) (list 0 (string-append (string-join committed "\n") "\n") "")]
      [(list "ls-files" "--others" "--exclude-standard")
       (list 0
             (if (null? untracked)
                 ""
                 (string-append (string-join untracked "\n") "\n"))
             "")]
      [(list "rev-list" _ ...) (list 0 "deadbeef\n" "")]
      [_
       (list 1
             ""
             (string-append "fake-git-facts: unhandled "
                            (string-join (map (lambda (a) (format "~a" a)) args) " ")
                            "\n"))])))

;; Run one delivery verification against a synthetic campaign with injected
;; Git facts and a fresh owned verification registry (deterministic per test).
(define (run-synthetic-verification base plan fake-git)
  (parameterize ([current-gsd-git-runner fake-git]
                 [current-gsd-verification-registry (make-verification-registry)])
    (run-delivery-verification base plan 0)))

;; ============================================================
;; Tests (decision claims on synthetic Git facts)
;; ============================================================

(define (decision-suite)
  (test-suite "delivery-verifier decision (synthetic Git facts)"

    ;; BUG-0068 secondary finding 2: markdown-decorated Verify criteria
    ;; (prose bullets + inline-code spans) must normalize into runnable
    ;; commands; canonical declarations pass through byte-for-byte.
    (test-case "normalize-declared-verify extracts code spans from prose bullets"
      (check-equal?
       (normalize-declared-verify
        "- `racket tests/test-shell-risk.rkt`, `racket tests/test-tool-bash-security.rkt` green.")
       "racket tests/test-shell-risk.rkt && racket tests/test-tool-bash-security.rkt"))
    (test-case "normalize-declared-verify joins one-command-per-bullet with &&"
      (check-equal? (normalize-declared-verify "- `racket a.rkt`\n- `racket b.rkt`")
                    "racket a.rkt && racket b.rkt"))
    (test-case "normalize-declared-verify passes canonical commands through unchanged"
      (define canonical "cd <project-base>/q && racket scripts/metrics.rkt --lint")
      (check-equal? (normalize-declared-verify canonical) canonical))
    (test-case "normalize-declared-verify passes plain commands through unchanged"
      (check-equal? (normalize-declared-verify "racket tests/test-foo.rkt")
                    "racket tests/test-foo.rkt"))

    (test-case "default delivery verify deadline is 14400s (bounded, multi-hour)"
      ;; Declared gates legitimately run for hours; the default deadline must
      ;; accommodate them while staying BOUNDED.
      (check-equal? (current-gsd-delivery-verify-timeout-sec) 14400))

    (test-case "approves when branch + files changed + verify passes"
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan (fake-git-facts)))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (check-true (pair? (delivery-verification-evidence result)))
      (cleanup-tmp base))

    (test-case "rejects when branch does not match expected issue branch"
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define result
        (run-synthetic-verification base
                                    plan
                                    (fake-git-facts #:branch "feature/issue-99-wave"))) ; wrong issue
      (check-false (delivery-verification-approved? result)
                   "wrong branch must fail delivery verification")
      (cleanup-tmp base))

    (test-case "approves committed delivery (wave committed + pushed + PR)"
      ;; A wave doc may instruct the agent to commit + push + open a PR. In
      ;; that flow the working tree is clean at verification time, so the
      ;; evidence is the commits on the current branch relative to its base
      ;; (origin/main or main), not an uncommitted working-tree diff.
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define result
        (run-synthetic-verification base
                                    plan
                                    (fake-git-facts #:head-changed '()
                                                    #:committed '("ui-core/preferences.rkt"))))
      (check-true (delivery-verification-approved? result)
                  "committed delivery on a feature branch must be approved")
      (cleanup-tmp base))

    (test-case "rejects when wave files unchanged"
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define result
        (run-synthetic-verification base plan (fake-git-facts #:head-changed '() #:committed '())))
      (check-false (delivery-verification-approved? result) "no delivery artifact must fail")
      (cleanup-tmp base))

    (test-case "approves directory target when a file under it changed"
      ;; A wave may scope a target to a DIRECTORY (e.g. "q/tests/memory/") for
      ;; "existing tests + new focused tests under this dir". git diff lists
      ;; files, never directories, so a directory target must be satisfied by
      ;; prefix: ANY changed file under the directory counts as delivery.
      (define base (make-synthetic-campaign!))
      (write-wave-doc! base 0 "zero" '("q/tests/memory/") "exit 0")
      (define plan (load-plan** base '("q/tests/memory/")))
      (define result
        (run-synthetic-verification base
                                    plan
                                    (fake-git-facts #:head-changed
                                                    '("tests/memory/policy-boundary-test.rkt"))))
      (check-true (delivery-verification-approved? result)
                  "directory target satisfied by a changed file under it")
      (cleanup-tmp base))

    (test-case "rejects directory target when nothing under it changed"
      (define base (make-synthetic-campaign!))
      (write-wave-doc! base 0 "zero" '("q/tests/memory/") "exit 0")
      (define plan (load-plan** base '("q/tests/memory/")))
      (define result
        (run-synthetic-verification base plan (fake-git-facts #:head-changed '() #:committed '())))
      (check-false (delivery-verification-approved? result)
                   "directory target with no changed file under it must fail")
      (cleanup-tmp base))

    (test-case "rejects when verify command fails"
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-git-runner (fake-git-facts)]
                       [current-gsd-verification-registry (make-verification-registry)]
                       [current-gsd-delivery-verify-command
                        "printf 'assertion expected 12 got 14\\n' >&2; exit 7"])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result)
                   "failing verify command must fail delivery verification")
      (define message (delivery-verification-message result))
      (check-true (string-contains? message "cmd=printf"))
      (check-true (string-contains? message "exit=7"))
      (check-true (string-contains? message "log="))
      (check-true (string-contains? message "failed-output-summary:"))
      (check-true (string-contains? message "assertion expected 12 got 14"))
      (cleanup-tmp base))

    (test-case "BUG-0058: coordinator gate emits started/completed progress events"
      (define base (make-synthetic-campaign!))
      (define plan (load-plan* base))
      (define-values (collector query) (make-event-collector))
      (define saved-bus (unbox gsd-event-bus-box))
      (set-gsd-event-bus! collector)
      (define result
        (parameterize ([current-gsd-git-runner (fake-git-facts)]
                       [current-gsd-verification-registry (make-verification-registry)]
                       [current-gsd-delivery-verify-command "exit 3"])
          (run-delivery-verification base plan 0)))
      (set-gsd-event-bus! saved-bus)
      (check-false (delivery-verification-approved? result))
      (define events (query))
      (define names (map (lambda (e) (hash-ref e 'event)) events))
      (check-true (and (member 'gsd.verification.started names) #t) (format "events: ~a" names))
      (check-true (and (member 'gsd.verification.completed names) #t) (format "events: ~a" names))
      (define started (findf (lambda (e) (eq? (hash-ref e 'event) 'gsd.verification.started)) events))
      (define completed
        (findf (lambda (e) (eq? (hash-ref e 'event) 'gsd.verification.completed)) events))
      (define started-data (hash-ref started 'data))
      (define completed-data (hash-ref completed 'data))
      (check-equal? (hash-ref started-data 'wave) 0)
      (check-equal? (hash-ref started-data 'timeout-sec) (current-gsd-delivery-verify-timeout-sec))
      (check-true (string? (hash-ref started-data 'log-path)) "started carries the durable log path")
      (check-equal? (hash-ref completed-data 'verdict) "reject")
      (check-equal? (hash-ref completed-data 'exit-code) 3)
      (check-true (real? (hash-ref completed-data 'elapsed-sec)) "elapsed is a real")
      (check-true (string-contains? (hash-ref completed-data 'reason) "state=")
                  "reject reason names the job state")
      (cleanup-tmp base))

    (test-case "approves issue-less campaign on main (no per-wave issues)"
      ;; A campaign that does not use per-wave GitHub issues runs on the
      ;; current branch (main) and has no STATE.md issue row. The branch
      ;; check must not invent an expected feature branch.
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
      ;; no write-state!: STATE.md absent → issue-less
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan (fake-git-facts #:branch "main")))
      (check-true (delivery-verification-approved? result)
                  "issue-less campaign on main must be approved")
      (check-false (or (regexp-match? #rx"expected=" (delivery-verification-message result)) #f)
                   "no spurious expected branch")
      (cleanup-tmp base))

    (test-case "ignores stale STATE.md issue row from a previous campaign"
      ;; STATE.md still lists the OLD campaign's issue mapping (different
      ;; wave-doc slug), so it must be treated as issue-less, not as a
      ;; feature/issue-N-wave expectation.
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
      ;; stale row linking a different wave doc (old slug "legacy")
      (call-with-output-file
       (build-path base ".planning" "STATE.md")
       (lambda (out)
         (display "| W0 | #42 | PENDING | [waves/W0-legacy.md](waves/W0-legacy.md) |\n" out))
       #:exists 'truncate)
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan (fake-git-facts #:branch "main")))
      (check-true (delivery-verification-approved? result)
                  "stale issue row referencing a different wave doc must be ignored")
      (cleanup-tmp base))

    (test-case "approves git-root-relative wave files (no q/ prefix)"
      ;; regression (W-campaign): wave docs may declare CI/workflow paths
      ;; git-root-relative (".github/workflows/ci.yml", "scripts/run-tests/...")
      ;; instead of repo-root-relative ("q/.github/..."). This mirrors the real
      ;; two-tier checkout: base-dir (/home/user/src/q-agent) is NOT a git repo;
      ;; the git root is base/q. The verifier must accept BOTH conventions — the
      ;; repo-root mapping of a git-root-relative declaration escapes the git
      ;; root ("../...") and must fall back to the declared path verbatim.
      (define base (make-synthetic-campaign!))
      (write-wave-doc! base
                       0
                       "zero"
                       (list ".github/workflows/full-regression.yml"
                             "scripts/run-tests/reporting.rkt")
                       "exit 0")
      (define plan
        (load-plan** base
                     (list ".github/workflows/full-regression.yml"
                           "scripts/run-tests/reporting.rkt")))
      (define result
        (run-synthetic-verification base
                                    plan
                                    (fake-git-facts #:head-changed
                                                    '(".github/workflows/full-regression.yml"
                                                      "scripts/run-tests/reporting.rkt"))))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (cleanup-tmp base))

    (test-case "verify gate accepts non-Racket wave targets (ci.yml, docs)"
      ;; Waves that touch .yml/.md alongside .rkt count as delivery; the
      ;; files-gate decision must not demand compilation of non-module files.
      ;; (The real compile-gate execution path — derived `raco make` fallback
      ;; — is owned by the git-contract owner's empty-declaration canary.)
      (define base (make-synthetic-campaign!))
      (write-wave-doc!
       base
       0
       "zero"
       (list "q/ui-core/preferences.rkt" "q/.github/workflows/ci.yml" "q/docs/reports/x.md")
       "exit 0")
      (define plan
        (load-plan**
         base
         (list "q/ui-core/preferences.rkt" "q/.github/workflows/ci.yml" "q/docs/reports/x.md")))
      (define result
        (run-synthetic-verification
         base
         plan
         (fake-git-facts #:head-changed '("ui-core/preferences.rkt" ".github/workflows/ci.yml"
                                                                    "docs/reports/x.md"))))
      (check-true (delivery-verification-approved? result)
                  "verify gate must ignore non-Racket changed files")
      (cleanup-tmp base))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (decision-suite))))))
