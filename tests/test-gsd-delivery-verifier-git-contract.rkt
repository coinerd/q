#lang racket/base
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120  ;; per-owner cap: measured cold median ~15s (the W2 measurement), 120s = default cap
;;
;; The campaign's W2 delivery-verifier boundary extraction — OWNER 2 of 3.
;;
;; REQUIRED real-Git fail-closed boundary canaries. Each test below proves a
;; boundary claim that CANNOT be proven on synthetic facts because the claim
;; is about real Git semantics or the fixture boundary itself:
;;   - the private Git fixture constructor contract (strategy-agnostic shape)
;;   - fail-closed when no Git repository exists (real rev-parse path)
;;   - real branch/commit/merge delivery shapes are recognized
;;   - the DECLARED wave verify command actually executes (real sh plane)
;;     from the base-dir cwd pin
;;   - the derived compile-gate fallback is described (real `raco make`)
;;
;; Decision-logic claims over synthetic Git facts live in the decision owner
;; (tests/test-gsd-delivery-verifier-decision.rkt); verify-gate execution +
;; coordinator composition live in the e2e owner
;; (tests/test-gsd-delivery-verifier-e2e.rkt). Ownership map:
;; docs/reports/DELIVERY-VERIFIER-SPLIT-v<q-version>.md.
;;
;; Timing provenance: the stale whole-file `@timeout 300` / `~123s` note from
;; the a2a10b9d pin (predecessor release) is superseded by per-owner caps
;; derived from W0/W2 repeatable measurements. No global timeout increase.
;;
;; tests/test-gsd-delivery-verifier-git-contract.rkt — real-Git boundary
;; canaries for the /go delivery verifier (fail-closed structured evidence).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         (only-in "helpers/private-fixture-templates.rkt"
                  make-private-git-fixture!
                  private-fixture-root
                  private-git-fixture-repo
                  private-fixture-kind
                  current-git-fixture-strategy
                  call-with-private-git-environment)
         (only-in "helpers/delivery-fixtures.rkt"
                  GIT
                  make-tmp-git-repo
                  write-plan!
                  write-wave-doc!
                  write-state!
                  load-plan*
                  load-plan**
                  make-git-file-change!
                  make-git-branch!
                  setup-standard-campaign!
                  cleanup-tmp)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message)
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt" make-verification-registry))

;; ============================================================
;; Tests (real-Git fail-closed boundary canaries)
;; ============================================================

(define (git-contract-suite)
  (test-suite "delivery-verifier git boundary contract (real Git)"

    ;; W2: this suite consumes git fixtures exclusively through the
    ;; shared `make-private-git-fixture!` constructor contract, so the
    ;; activated 'pristine-copy strategy must be invisible to every consumer.
    ;; Contract test: both strategies yield the same consumer-visible shape
    ;; (kind, repo dir, resolvable HEAD, self-contained object store).
    (test-case "git fixture constructor contract is strategy-agnostic"
      (define (shape)
        (define tmp (make-temporary-file "dv-strategy-~a" 'directory))
        (define fx (make-private-git-fixture! #:parent-root tmp #:tag "dv-shape"))
        (define repo (private-git-fixture-repo fx))
        (begin0 (list (private-fixture-kind fx)
                      (path? (private-fixture-root fx))
                      (directory-exists? (build-path repo ".git" "objects"))
                      (zero? (system*/exit-code (find-executable-path "git")
                                                "-C"
                                                (path->string repo)
                                                "rev-parse"
                                                "--verify"
                                                "HEAD")))
          (delete-directory/files tmp #:must-exist? #f)))
      (define pristine-shape
        (parameterize ([current-git-fixture-strategy 'pristine-copy])
          (shape)))
      (define legacy-shape
        (parameterize ([current-git-fixture-strategy 'legacy-clone])
          (shape)))
      (check-equal? pristine-shape legacy-shape)
      (check-equal? pristine-shape (list 'git #t #t #t)))

    (test-case "approves committed delivery (wave committed + pushed + PR)"
      ;; REAL-Git canary: a committed feature-branch delivery whose working
      ;; tree is clean must be recognized via the base-relative commit diff,
      ;; and the declared verify must run as a real `raco make`.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      ;; commit the change on the feature branch (no uncommitted diff)
      (make-git-file-change! base)
      ;; git commands must run inside the repo (base/q), not the base dir
      (parameterize ([current-directory (build-path base "q")])
        (system*/exit-code GIT "add" "-A")
        (system*/exit-code GIT "commit" "-q" "-m" "wave delivery"))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result)
                  "committed delivery on a feature branch must be approved")
      (check-true (pair? (delivery-verification-evidence result)))
      (cleanup-tmp base))

    (test-case "rejects when git not available"
      ;; REAL-Git fail-closed canary: an absent repository must reject the
      ;; wave — never approve silently. The fixture has NO git repo on disk,
      ;; so this exercises the genuine rev-parse failure path.
      (define base (make-temporary-file "dv-nogit-~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? result) "no git repository must fail closed")
      (cleanup-tmp base))

    (test-case "approves merged-to-main delivery via campaign created-at"
      ;; Regression: the wave agent committed + merged its work to
      ;; main, so HEAD == origin/main and the base-relative diff is empty. The
      ;; verifier must recognize delivery when the wave target files changed in
      ;; commits since the campaign's creation time (the campaign base). Without
      ;; created-at the wave is rejected; with it the wave is approved.
      (define base (make-temporary-file "dv-merged-~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (make-directory* (build-path base "q" ".github" "workflows"))
      (make-directory* (build-path base "q" "scripts" "run-tests"))
      (define (sh . args)
        (define exit
          (parameterize ([current-directory (build-path base "q")])
            (apply system*/exit-code GIT args)))
        (unless (zero? exit)
          (error 'merged "command failed: ~a" (cons 'sh args))))
      (sh "init" "-q" ".")
      (sh "config" "user.email" "test@example.com")
      (sh "config" "user.name" "Test")
      (sh "checkout" "-q" "-b" "main")
      ;; baseline commit BEFORE the campaign
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display
          "#lang racket/base\n(provide write-json-results!)\n(define (write-json-results! p) p)\n"
          out))
       #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "baseline")
      ;; campaign created-at = now (before the wave work)
      (define created-at (current-seconds))
      (sleep 2)
      ;; wave work committed + merged to main (HEAD == origin/main after merge)
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression-v2\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display (string-append
                   "#lang racket/base\n"
                   "(require racket/path racket/file)\n"
                   "(provide write-json-results!)\n"
                   "(define (write-json-results! p) (when p (make-directory* (path-only p))))\n")
                  out))
       #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "W0: repair full-regression evidence path")
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       (list ".github/workflows/full-regression.yml"
                             "scripts/run-tests/reporting.rkt")
                       "raco make q/scripts/run-tests/reporting.rkt")
      ;; no write-state!: issue-less campaign, work on main
      (define plan
        (load-plan** base
                     (list ".github/workflows/full-regression.yml"
                           "scripts/run-tests/reporting.rkt")))
      (define without (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? without)
                   "without created-at, merged-to-main work is not visible (HEAD == base)")
      (define with-created (run-delivery-verification base plan 0 created-at))
      (check-true (delivery-verification-approved? with-created)
                  (delivery-verification-message with-created))
      (cleanup-tmp base))

    (test-case "declared wave verify command executes when no override is bound"
      ;; Truthful verification: without an explicit override the verifier runs
      ;; the wave's DECLARED verify command (doc `## Verify` — the source
      ;; gsd-wave-verify is built from), not a silently derived gate. The
      ;; marker file proves the declared command actually executed.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (define marker
        (build-path (find-system-path 'temp-dir)
                    (format "dv-declared-~a.marker" (current-inexact-milliseconds))))
      (with-handlers ([exn:fail? void])
        (delete-file marker))
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       (format "echo declared-verify-ran > ~a" marker))
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (check-true (file-exists? marker) "the DECLARED command executed")
      (delete-file marker)
      (cleanup-tmp base))

    (test-case "declared verify command runs from the base-dir project root"
      ;; Declared commands are authored against the PLAN.md/.planning layout
      ;; ("q/…"-prefixed targets), so the two-tier checkout must resolve them
      ;; from base-dir even though the git root is <base>/q.
      ;; Include spaces so placeholder expansion must shell-quote the path.
      (define base (make-temporary-file "dv cwd ~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (make-directory* (build-path base "q" "scripts" "run-tests"))
      (define (sh . args)
        (define exit
          (parameterize ([current-directory (build-path base "q")])
            (apply system*/exit-code GIT args)))
        (unless (zero? exit)
          (error 'cwd-pin "command failed: ~a" (cons 'sh args))))
      (sh "init" "-q" ".")
      (sh "config" "user.email" "test@example.com")
      (sh "config" "user.name" "Test")
      (sh "checkout" "-q" "-b" "main")
      (call-with-output-file (build-path base "q" "scripts" "run-tests" "reporting.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide w)\n(define w 1)\n" out))
                             #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "baseline")
      ;; modify the target so the files gate passes
      (call-with-output-file (build-path base "q" "scripts" "run-tests" "reporting.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide w)\n(define w 2)\n" out))
                             #:exists 'truncate)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("scripts/run-tests/reporting.rkt")
                       "cd <project-base>/q && test -f scripts/run-tests/reporting.rkt")
      (define plan (load-plan** base '("scripts/run-tests/reporting.rkt")))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result)
                  (format "base-dir cwd must resolve the <project-base>/q declaration: ~a"
                          (delivery-verification-message result)))
      (cleanup-tmp base))

    (test-case "empty verify declaration: compile gate runs as a separately described fallback"
      ;; A genuinely EMPTY declaration (doc `## Verify` empty and plan verify
      ;; empty) falls back to the derived compile gate — and the fallback is
      ;; DESCRIBED in the evidence, never a silent substitute.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "")
      (write-state! base 0 "42")
      (define plan (load-plan** base '("q/ui-core/preferences.rkt") ""))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (define verify-detail (cdr (cdr (assoc "verify" (delivery-verification-evidence result)))))
      (check-true (string-contains? verify-detail "compile-gate fallback")
                  (format "fallback must be separately described: ~a" verify-detail))
      (check-true (string-contains? verify-detail "raco make")
                  "the derived gate command is visible in the evidence")
      (cleanup-tmp base))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (git-contract-suite))))))
