#lang racket/base
;; @covers extensions/gsd/plan-types-parser.rkt
;; @covers extensions/gsd/verify-command-format.rkt
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;; @timeout 120
;;
;; BUG-0075 (R75) — complete non-vacuous Verify declaration contract.
;;
;; One suite owns the END-TO-END declaration path so the layers cannot
;; drift: parser (section grammar preserved, structural refusals named) ->
;; extraction/normalization (COMPLETE validated command list; every authored
;; span becomes an executed marker or a named rejection carrying the span;
;; zero accepted commands and malformed fences refuse without compile
;; fallback) -> execution (N declared gates => N executed markers; English
;; prose and bullets are never launched through /bin/sh).
;;
;; Pure classification claims live here on strings; execution claims run the
;; real owned verification registry over SYNTHETIC Git facts (the decision
;; owner's fixture style), so no branch/commit/working-tree churn happens
;; beyond the one `git init` per fixture.

(require rackunit
         rackunit/text-ui
         racket/file
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
         (only-in "../extensions/gsd/plan-types.rkt" parse-wave-content)
         (only-in "../extensions/gsd/verify-command-format.rkt"
                  verify-declaration-extraction
                  verify-command-stages
                  verify-normalizer-version)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  normalize-declared-verify
                  run-delivery-verification
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message
                  current-gsd-delivery-verify-command
                  current-gsd-git-runner)
         (only-in "../extensions/gsd/composition-root.rkt"
                  current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt" make-verification-registry)
         (only-in "helpers/private-fixture-templates.rkt" call-with-private-git-environment))

;; ============================================================
;; Fixtures (decision-owner style: one git init, fake Git facts)
;; ============================================================

(define (make-synthetic-repo)
  (define base (make-temporary-file "vdc-synth-~a" 'directory))
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

(define (fake-git-facts)
  (lambda (git-root args)
    (match args
      [(list "rev-parse" "--is-inside-work-tree") (list 0 "true\n" "")]
      [(list "rev-parse" "--abbrev-ref" "HEAD") (list 0 "feature/issue-42-wave\n" "")]
      [(list "rev-parse" "--verify" _) (list 0 "main\n" "")]
      [(list "rev-parse" _ ...) (list 0 "deadbeef\n" "")]
      [(list "diff" "--name-only" "HEAD")
       (list 0 "ui-core/preferences.rkt\n" "")]
      [(list "diff" "--name-only" _)
       (list 0 "ui-core/preferences.rkt\n" "")]
      [(list "ls-files" "--others" "--exclude-standard") (list 0 "" "")]
      [(list "rev-list" _ ...) (list 0 "deadbeef\n" "")]
      [_ (list 1 "" "fake-git-facts: unhandled invocation\n")])))

(define (run-synthetic-verification base plan)
  (parameterize ([current-gsd-git-runner (fake-git-facts)]
                 [current-gsd-verification-registry (make-verification-registry)])
    (run-delivery-verification base plan 0)))

(define (verify-check result)
  (define entry (assoc "verify" (delivery-verification-evidence result)))
  (and entry (cdr entry)))

(define (verify-evidence result)
  (define check (verify-check result))
  (and check (cdr check)))

;; ============================================================
;; Extraction vocabulary (pure)
;; ============================================================

(define (extraction d)
  (verify-declaration-extraction d))

(define (commands-of d) (hash-ref (extraction d) 'commands))
(define (canonical-of d) (hash-ref (extraction d) 'canonical))
(define (rejections-of d) (hash-ref (extraction d) 'rejections))
(define (refusal-of d) (hash-ref (extraction d) 'refusal))

(define (contract-suite)
  (test-suite "verify declaration contract (BUG-0075)"

    ;; ------------------------------------------------------------
    ;; prose-before-bullets: parser -> normalizer -> execution
    ;; ------------------------------------------------------------
    (test-case "parser preserves prose-before-bullets grammar (no '; ' flattening)"
      (define content
        (string-append
         "## Verify\n"
         "The gates below must all pass before delivery.\n"
         "- `racket scripts/run-tests.rkt --suite fast` green\n"
         "- `raco fmt --check q/ui-core/preferences.rkt` clean\n"
         "\n## Done\n- delivered\n"))
      (define parsed (parse-wave-content content))
      (define verify (hash-ref parsed 'verify))
      ;; Bullet anchors must survive into the declaration: the normalizer's
      ;; decorated? sees bullets after prose only when newlines are preserved.
      (check-true (string-contains? verify "\n- `racket scripts/run-tests.rkt")
                  (format "section grammar flattened: ~a" verify))
      (check-false (string-contains? verify "; "))
      (check-false (hash-ref parsed 'verify-refusal #f))
      ;; ... and the normalizer therefore extracts the two commands, never
      ;; handing English to the shell.
      (define canonical (normalize-declared-verify verify))
      (check-equal? canonical
                    "racket scripts/run-tests.rkt --suite fast && raco fmt --check q/ui-core/preferences.rkt")
      (check-false (string-contains? canonical "The gates below")))

    (test-case "extraction: prose-before-bullets yields the complete command list"
      (define declaration
        (string-append
         "The gates below must all pass.\n"
         "- `racket scripts/run-tests.rkt --suite fast` green; `metrics.rkt --lint` green."))
      (check-equal? (commands-of declaration)
                    (list "racket scripts/run-tests.rkt --suite fast"
                          "racket scripts/metrics.rkt --lint"))
      (check-equal? (canonical-of declaration)
                    "racket scripts/run-tests.rkt --suite fast && racket scripts/metrics.rkt --lint")
      (check-equal? (rejections-of declaration) '())
      (check-false (refusal-of declaration)))

    ;; ------------------------------------------------------------
    ;; N declared gates => N executed markers
    ;; ------------------------------------------------------------
    (test-case "N declared gates produce N canonical markers in document order"
      (define declaration
        (string-append
         "- `raco make a.rkt`\n"
         "- `raco fmt --check a.rkt`\n"
         "- `racket tests/a.rkt`\n"
         "- `racket scripts/run-tests.rkt --suite fast`\n"))
      (define canonical (normalize-declared-verify declaration))
      (define stages (verify-command-stages canonical))
      (check-equal? (length stages) 4)
      (check-equal? stages
                    (list "raco make a.rkt"
                          "raco fmt --check a.rkt"
                          "racket tests/a.rkt"
                          "racket scripts/run-tests.rkt --suite fast"))
      (check-equal? (commands-of declaration) stages)
      (check-equal? (length (commands-of declaration))
                    (length (verify-command-stages (canonical-of declaration)))))

    (test-case "execution of a 3-gate declaration runs all 3 markers and names gates=3"
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc!
       base
       0
       "zero"
       '("q/ui-core/preferences.rkt")
       (string-append
        "- `bash -c 'printf a > marker-a.txt'` first\n"
        "- `bash -c 'test -f marker-a.txt && printf b > marker-b.txt'` second\n"
        "- `bash -c 'test -f marker-b.txt'` third\n"))
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan))
      (check-true (delivery-verification-approved? result)
                  (delivery-verification-message result))
      (check-true (file-exists? (build-path base "q" "marker-a.txt"))
                  "gate 1 executed (fail-fast && chain)")
      (check-true (file-exists? (build-path base "q" "marker-b.txt"))
                  "gate 2 executed after gate 1")
      (define detail (verify-evidence result))
      (check-true (string-contains? detail "gates=3") (format "detail: ~a" detail))
      (for ([marker (list "printf a > marker-a.txt"
                          "test -f marker-a.txt && printf b > marker-b.txt"
                          "test -f marker-b.txt")])
        (check-true (string-contains? detail marker) (format "detail: ~a" detail)))
      (cleanup-tmp base))

    ;; ------------------------------------------------------------
    ;; Unsupported command-shaped spans reject LOUDLY (never silent drops)
    ;; ------------------------------------------------------------
    (test-case "unsupported span among valid gates is a named rejection carrying the span"
      (define declaration
        (string-append
         "- `racket tests/a.rkt` green\n"
         "- `grep -L forbidden q/report.txt` clean\n"))
      (define rejections (rejections-of declaration))
      (check-equal? (length rejections) 1)
      (check-equal? (hash-ref (car rejections) 'reason) 'grep-exit-semantics)
      (check-equal? (hash-ref (car rejections) 'span) "grep -L forbidden q/report.txt")
      (check-equal? (canonical-of declaration) "")
      (check-equal? (normalize-declared-verify declaration) "")
      ;; the supported span is NOT silently half-run either: canonical is empty
      (check-equal? (commands-of declaration) '()))

    (test-case "execution refuses an unsupported span visibly; nothing is launched"
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc!
       base
       0
       "zero"
       '("q/ui-core/preferences.rkt")
       (string-append
        "- `bash -c 'printf x > marker-x.txt'` valid gate\n"
        "- `find q -name '*.rkt'` unsupported gate\n"))
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan))
      (check-false (delivery-verification-approved? result))
      (define message (delivery-verification-message result))
      (check-true (string-contains? message "unsupported-command")
                  (format "message: ~a" message))
      (check-true (string-contains? message "find q -name '*.rkt'")
                  (format "the original span must be named: ~a" message))
      (check-false (file-exists? (build-path base "q" "marker-x.txt"))
                   "the valid span must not half-run when a sibling span is rejected")
      (cleanup-tmp base))

    (test-case "grep spans are refused, never rewritten to an inverted grep"
      (define declaration "- `grep -q forbidden q/report.txt` clean\n")
      (check-equal? (hash-ref (car (rejections-of declaration)) 'reason)
                    'grep-exit-semantics)
      (check-equal? (canonical-of declaration) "")
      ;; no auto-rewrite: nothing containing "! grep" is produced anywhere
      (check-false (string-contains? (canonical-of declaration) "! grep")))

    ;; ------------------------------------------------------------
    ;; Zero accepted commands fails; malformed fence refuses w/o fallback
    ;; ------------------------------------------------------------
    (test-case "zero accepted commands is a named refusal, not a compile fallback"
      (define declaration "- criterion one.\n- criterion two.\n")
      (check-equal? (refusal-of declaration) 'no-executable-command)
      (check-equal? (canonical-of declaration) ""))

    (test-case "execution refuses prose-only bullets without launching or falling back"
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt")
                       "- criterion one.\n- criterion two.")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan))
      (check-false (delivery-verification-approved? result))
      (define message (delivery-verification-message result))
      (check-true (string-contains? message "refusing prose-as-shell")
                  (format "message: ~a" message))
      (check-false (string-contains? message "compile-gate")
                   "zero accepted commands must not fall through to the compile gate")
      (cleanup-tmp base))

    (test-case "parser names structural refusals: unclosed fence and non-shell fence"
      (define unclosed "## Verify\nRun this:\n```sh\nraco test x.rkt\n")
      (define parsed (parse-wave-content unclosed))
      (check-equal? (hash-ref parsed 'verify) "")
      (check-true (string-contains? (hash-ref parsed 'verify-refusal "") "unclosed")
                  (format "refusal: ~a" (hash-ref parsed 'verify-refusal)))
      (define nonshell "## Verify\nRun this:\n```json\n{}\n```\n## Done\nfinished\n")
      (define parsed2 (parse-wave-content nonshell))
      (check-equal? (hash-ref parsed2 'verify) "")
      (check-true (string-contains? (hash-ref parsed2 'verify-refusal "") "shell")
                  (format "refusal: ~a" (hash-ref parsed2 'verify-refusal))))

    (test-case "malformed fence refuses WITHOUT the compile-gate fallback"
      (define base (make-synthetic-repo))
      (write-plan! base 0 "Wave Zero" "zero")
      ;; The Racket target changed, so the compile gate WOULD be derivable —
      ;; the malformed declaration must refuse instead.
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "```sh\ntest -f q/ui-core/preferences.rkt\n")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-synthetic-verification base plan))
      (check-false (delivery-verification-approved? result))
      (define message (delivery-verification-message result))
      (check-true (string-contains? message "unclosed") (format "message: ~a" message))
      (check-false (string-contains? message "compile-gate")
                   "malformed declarations must not reach the compile-gate fallback")
      (cleanup-tmp base))

    ;; ------------------------------------------------------------
    ;; grep / test / bracket decisions
    ;; ------------------------------------------------------------
    (test-case "test and POSIX bracket spans are explicitly accepted"
      (check-equal? (commands-of "- `test -f q/ui-core/preferences.rkt` exists")
                    (list "test -f q/ui-core/preferences.rkt"))
      (check-equal? (commands-of "- `[ -f q/ui-core/preferences.rkt ]` exists")
                    (list "[ -f q/ui-core/preferences.rkt ]"))
      (check-equal? (canonical-of "- `test -f a.rkt`\n- `[ -f b.rkt ]`")
                    "test -f a.rkt && [ -f b.rkt ]"))

    (test-case "bash-only bracket and inverted spans are named rejections"
      (check-equal? (hash-ref (car (rejections-of "- `[[ -f q/x.rkt ]]` exists"))
                            'reason)
                    'bash-only-construct)
      (check-equal? (hash-ref (car (rejections-of "- `! grep -q x q/x.txt` clean"))
                            'reason)
                    'inverted-command)
      (check-equal? (canonical-of "- `[[ -f q/x.rkt ]]`") ""))

    (test-case "negative-content gates distinguish present/absent/missing/unreadable"
      ;; The supported explicit form for a negative content gate is a count
      ;; composition inside an explicit bash invocation (the retained wave
      ;; pattern): `test "$(grep -c TOKEN file)" -eq 0`. Its four outcomes
      ;; are distinguishable — forbidden PRESENT fails, ABSENT passes,
      ;; MISSING file fails (grep exits 2 with empty output, so the test
      ;; expression errors nonzero), UNREADABLE fails — unlike a bare or
      ;; `!`-inverted grep, whose exit-2 error is indistinguishable from (or
      ;; inverts into) an apparent pass.
      (define (negative-gate-check probe-exists? forbidden-present? unreadable?)
        (define base (make-synthetic-repo))
        (dynamic-wind
         void
         (lambda ()
           (define probe (build-path base "q" "probe.txt"))
           (when probe-exists?
             (display-to-file (if forbidden-present? "has FORBIDDENTOKEN here\n" "clean text\n")
                              probe
                              #:exists 'truncate))
           (when unreadable? (file-or-directory-permissions probe #o000))
           (write-plan! base 0 "Wave Zero" "zero")
           (write-wave-doc!
            base
            0
            "zero"
            '("q/ui-core/preferences.rkt")
            (string-append
             "- `bash -c 'test \"$(grep -c FORBIDDENTOKEN probe.txt)\" -eq 0'`"
             " no forbidden token\n"))
           (write-state! base 0 "42")
           (define plan (load-plan* base))
           (define result (run-synthetic-verification base plan))
           (verify-check result))
         (lambda ()
           (when unreadable? (file-or-directory-permissions (build-path base "q" "probe.txt") #o644))
           (cleanup-tmp base))))
      (check-true (car (negative-gate-check #t #f #f)) "absent forbidden token must pass")
      (check-false (car (negative-gate-check #t #t #f)) "present forbidden token must fail")
      (check-false (car (negative-gate-check #f #f #f)) "missing probe file must fail")
      (check-false (car (negative-gate-check #t #f #t)) "unreadable probe file must fail"))

    ;; ------------------------------------------------------------
    ;; Quoted grep-looking prose must not false-ban
    ;; ------------------------------------------------------------
    (test-case "prose identifiers and quoted examples are never banned"
      (define declaration
        (string-append
         "- `unknown` stays unknown; classification is `distinct_*`.\n"
         "- the token `R2 PASSED` must not appear; see `q/foo.rkt`.\n"
         "- `bash -c 'test \"$(grep -c \"Not an R2 pass\" q/report.md)\" -ge 1'` mandated\n"))
      (check-equal? (rejections-of declaration) '())
      (check-equal? (commands-of declaration)
                    (list "bash -c 'test \"$(grep -c \"Not an R2 pass\" q/report.md)\" -ge 1'")))

    ;; ------------------------------------------------------------
    ;; Bullet-first / legacy single-line compatibility (retained authority)
    ;; ------------------------------------------------------------
    (test-case "bullet-first sections normalize to the pinned && chain"
      (check-equal? (normalize-declared-verify "- `racket a.rkt`\n- `racket b.rkt`")
                    "racket a.rkt && racket b.rkt")
      (check-equal?
       (normalize-declared-verify
        "- `racket tests/test-shell-risk.rkt`, `racket tests/test-tool-bash-security.rkt` green.")
       "racket tests/test-shell-risk.rkt && racket tests/test-tool-bash-security.rkt"))

    (test-case "legacy single-line shell contracts pass through byte-for-byte"
      (check-equal? (normalize-declared-verify "exit 0") "exit 0")
      (check-equal? (normalize-declared-verify "racket tests/test-foo.rkt")
                    "racket tests/test-foo.rkt")
      (check-equal? (normalize-declared-verify "cd <project-base>/q && racket scripts/metrics.rkt --lint")
                    "cd <project-base>/q && racket scripts/metrics.rkt --lint")
      ;; metrics shorthand canonicalizes only as a bullet span; a bare legacy
      ;; single line passes through byte-for-byte
      (check-equal? (normalize-declared-verify "metrics.rkt --lint") "metrics.rkt --lint")
      (check-equal? (normalize-declared-verify "- `metrics.rkt --lint` green")
                    "racket scripts/metrics.rkt --lint")
      ;; retained review-proof authority: unfenced single-line compound shell
      (define compound "i=0; while [ $i -lt 9000 ]; do printf '0123456789abcdef\\n'; i=$((i+1)); done")
      (check-equal? (normalize-declared-verify compound) compound))

    (test-case "retained W0 bash -c gate form normalizes byte-for-byte"
      (define declaration
        (string-append
         "- `bash -c 'test -s q/docs/reports/CLOSURE.md -a -s q/docs/reports/PLAN.md'`\n"
         "- `bash -c 'git -C q ls-files --error-unmatch docs/reports/CLOSURE.md >/dev/null'`\n"
         "- `bash -c 'test \"$(grep -c \"Not an R2 pass\" q/docs/reports/CLOSURE.md)\" -ge 1'`\n"
         "- `bash -c '! grep -q \"R2 PASSED\" q/docs/reports/CLOSURE.md'`\n"))
      (check-equal?
       (normalize-declared-verify declaration)
       (string-append
        "bash -c 'test -s q/docs/reports/CLOSURE.md -a -s q/docs/reports/PLAN.md'"
        " && bash -c 'git -C q ls-files --error-unmatch docs/reports/CLOSURE.md >/dev/null'"
        " && bash -c 'test \"$(grep -c \"Not an R2 pass\" q/docs/reports/CLOSURE.md)\" -ge 1'"
        " && bash -c '! grep -q \"R2 PASSED\" q/docs/reports/CLOSURE.md'")))

    (test-case "fenced shell keeps newlines and canonicalizes to the audited && grammar"
      (define declaration
        "```sh\nraco make a.rkt\nracket tests/a.rkt\nracket scripts/run-tests.rkt --suite fast\n```")
      (define content-decl "raco make a.rkt\nracket tests/a.rkt\nracket scripts/run-tests.rkt --suite fast")
      (check-equal? (normalize-declared-verify content-decl)
                    (string-replace content-decl "\n" " && "))
      ;; heredoc/compound content is preserved by the parser byte-for-byte
      (define script "# compile\nraco make \\\n  x.rkt\ncat <<'EOF'\n\n## literal, not a heading\nEOF")
      (check-equal? (hash-ref (parse-wave-content
                               (string-append "## Verify\n```sh\n" script "\n```\n## Done\nfinished\n"))
                              'verify)
                    script))

    (test-case "retained fenced printf authority keeps its pinned canonical form"
      ;; The parser strips the fence; the audited content canonicalizes to
      ;; the fail-fast && chain (the review-proof owner freezes this form).
      (define content "printf 'owned-output\\n'\nprintf 'second-stage\\n'")
      (check-equal? (normalize-declared-verify content)
                    "printf 'owned-output\\n' && printf 'second-stage\\n'")
      (check-equal?
       (hash-ref (parse-wave-content "## Verify\n```sh\nprintf 'owned-output\\n'\nprintf 'second-stage\\n'\n```\n")
                 'verify)
       content))

    (test-case "ambiguous unfenced multiline grammar still refuses conservatively"
      (define bad "raco make a.rkt &&\nracket tests/a.rkt")
      (check-equal? (normalize-declared-verify bad) "")
      (check-equal? (refusal-of bad) 'ambiguous-grammar)
      (check-equal? (normalize-declared-verify "printf 'quoted && delimiter'\nracket tests/a.rkt") "")
      (check-equal? (normalize-declared-verify "raco make a.rkt && && racket tests/a.rkt") ""))

    (test-case "English prose lines in unfenced multiline sections are never launched"
      (define declaration "Run this:\nraco test x.rkt")
      (check-equal? (canonical-of declaration) "")
      (check-equal? (hash-ref (car (rejections-of declaration)) 'reason)
                    'prose-in-unfenced-multiline)
      (check-equal? (hash-ref (car (rejections-of declaration)) 'span) "Run this:"))

    (test-case "the normalizer version is pinned and flows into extraction consumers"
      (check-equal? verify-normalizer-version 2))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (contract-suite))))))
