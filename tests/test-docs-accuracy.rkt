#lang racket

;; tests/test-docs-accuracy.rkt
;; Docs-accuracy pins for docs/testing.md (final plan §7, Wave F): every
;; `./qt` command and flag quoted in the quick-start must resolve to the
;; implemented CLI surface (`qt` + `scripts/testing/full-plan.rkt`), the
;; honesty pass-line labels must match full-plan.rkt's own pinned labels and
;; patterns, the required-offline lane must be the exact suite union the doc
;; names, and every `scripts/run-tests.rkt` compatibility consumer named in
;; the doc must still exist and use the adapter. A doc claim that drifts from
;; the code is a red test; the doc moves in the same commit as its contract.
;; Pure file reads only — no suite, lane or release command is executed.

;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         "../scripts/testing/full-plan.rkt"
         (only-in "../scripts/testing/quick-audit.rkt" admitted-owners))

(define-runtime-path doc-path "../docs/testing.md")
(define-runtime-path qt-path "../qt")
(define-runtime-path root "..")
(define-runtime-path compat-path "../scripts/run-tests.rkt")
(define-runtime-path runner-cli-path "../scripts/testing/runner-cli.rkt")
(define-runtime-path quick-audit-path "../scripts/testing/quick-audit.rkt")
(define-runtime-path preparation-path "../scripts/testing/preparation.rkt")
(define-runtime-path runner-path "../scripts/run-tests/runner.rkt")
(define-runtime-path admission-record "../tests/.quick-admission.rktd")
(define-runtime-path changed-selection-path "../scripts/testing/changed-selection.rkt")

(define doc (file->string doc-path))
;; Collapsed view for claims the doc wraps across lines.
(define flat (string-normalize-spaces doc))
(define qt-text (file->string qt-path))
(define compat-text (string-append (file->string compat-path) (file->string runner-cli-path)))

;; The flag surface `qt` implements (qt source grammar); the doc may only
;; quote flags from this set. Commands likewise come from the qt dispatch.
(define qt-commands '("quick" "default" "full" "release" "help"))
(define qt-flags
  '("--changed" "--explain"
                "--audit"
                "--jobs"
                "--sequential"
                "--json-out"
                "--suite"
                "--shard-index"
                "--shard-total"
                "--timeout"
                "--profile"
                "--ledger"
                "--record-gate-evidence"
                "--repeat"
                "--mode"
                "--shard-plan"
                "--durations"
                "--lane"
                "--candidate"
                "--evidence"
                "--file"))
;; Flags whose following word is a value. `--changed` takes an optional value,
;; so it is NOT listed: its value flows through the positional check instead.
(define qt-value-flags '("--lane" "--candidate" "--evidence" "--file" "--jobs"))

;; PLACEHOLDER, example path, or a concrete commit SHA — never checked further.
(define (placeholder? w)
  (or (string-prefix? w "<")
      (regexp-match? #rx"^[A-Z][A-Z._-]*$" w)
      (regexp-match? #rx"^[0-9a-f]{7,40}$" w)
      (regexp-match? #rx"^tests/test-[^/]+[.]rkt$" w)))

;; Every `./qt ...` token quoted in the doc, backticks stripped.
(define (doc-qt-tokens text)
  (for/list ([m (in-list (regexp-match* #rx"`[.]/qt[^`]*`" text))])
    (string-trim m "`")))

;; List of problems for one token: unknown command, unknown flag, or a
;; --lane value that is not a real lane (or a placeholder).
(define (qt-token-problems tok lane-names)
  (define words (string-split tok))
  (define problems '())
  (define (bad! w why)
    (set! problems (cons (format "~a: ~a" w why) problems)))
  (when (or (null? words) (not (equal? (car words) "./qt")))
    (bad! tok "token must start with ./qt"))
  (define rest
    (if (null? words)
        '()
        (cdr words)))
  (define first-len (length rest))
  (let loop ([ws rest]
             [pending #f])
    (unless (null? ws)
      (define w (car ws))
      (cond
        [pending
         (when (and (equal? pending "--lane") (not (string-prefix? w "-")))
           (unless (or (member w lane-names) (placeholder? w))
             (bad! w "not a qt lane name")))
         (loop (cdr ws) #f)]
        [(string-prefix? w "-")
         (cond
           [(member w '("--help" "-h")) (loop (cdr ws) #f)]
           [(member w qt-flags)
            (define takes-value?
              (and (member w qt-value-flags) (pair? (cdr ws)) (not (string-prefix? (cadr ws) "-"))))
            (loop (cdr ws) (and takes-value? w))]
           [else
            (bad! w "not an implemented ./qt flag")
            (loop (cdr ws) #f)])]
        [(= (length ws) first-len)
         (unless (member w qt-commands)
           (bad! w "not an implemented ./qt command"))
         (loop (cdr ws) #f)]
        [else
         (unless (placeholder? w)
           (bad! w "unexpected positional argument"))
         (loop (cdr ws) #f)])))
  (reverse problems))

(module+ test
  (test-case "docs/testing.md exists and carries the Wave F scope marks"
    (check-true (file-exists? doc-path) "the quick-start exists")
    (check-regexp-match #rx"(?i:measured on the legacy CLI)" doc)
    (check-true (string-contains? doc "The former L1/L2 impact levels of this guide are retired"))
    (check-false (string-contains? doc "## The L0–L3 loop") "the legacy ladder heading is gone")
    (for ([tok '("`./qt quick`" "`./qt quick --changed BASE..HEAD`" "`./qt default`" "`./qt full`")])
      (check-true (string-contains? doc tok) (format "command column names ~a" tok))))

  (test-case "every ./qt token quoted in the doc resolves to the implemented CLI"
    (define lane-names (map (lambda (l) (symbol->string (qt-lane-name l))) qt-lanes))
    (define tokens (doc-qt-tokens doc))
    (check-true (pair? tokens) "the doc quotes ./qt invocations")
    (for ([tok (in-list tokens)])
      (check-equal? (qt-token-problems tok lane-names)
                    '()
                    (format "~a must resolve against the qt CLI" tok))))

  (test-case "the doc's pass-line labels match full-plan.rkt's pinned labels"
    ;; quick: the labels are literal strings inside the qt script
    (check-true (string-contains? flat "quick passed: N files, M tests (not PR/full verification)"))
    (check-true (string-contains? qt-text "not PR/full verification"))
    (check-true (string-contains? qt-text "changed-file impact reproduction only, not a PR pass"))
    ;; full: the doc's label must equal the exported label, and the pattern
    ;; must accept the code's own format output (self-consistency)
    (check-equal? full-label "full Racket suite only, not PR/CI verification")
    (check-true (string-contains?
                 flat
                 "full passed: N files, M tests (full Racket suite only, not PR/CI verification)"))
    (check-regexp-match full-pass-pattern (format "full passed: 3 files, 7 tests (~a)" full-label))
    ;; default: the doc quotes the line head; the pattern is checked against
    ;; the code's own format output with the exported label
    (check-equal? default-label
                  "offline PR verification only; platform and GitHub-hosted claims remain with CI")
    (check-true
     (string-contains?
      flat
      "default passed: N files, M tests (offline PR verification; K unavailable-here listed for CI"))
    (check-regexp-match
     default-pass-pattern
     (format
      "default passed: 1 files, 2 tests (offline PR verification; 3 unavailable-here listed for CI; ~a)"
      default-label))
    ;; release: the doc names the stable obligation slugs and the blocking rule
    (define slugs (map release-obligation-slug (release-obligations)))
    (check-equal? slugs '(governance-parity ci-verification branch-protection release-provenance))
    (for ([slug (in-list slugs)])
      (check-true (string-contains? flat (symbol->string slug)) (format "the doc names ~a" slug)))
    (check-true (string-contains? flat "never inferred"))
    (check-true (string-contains? flat "exits non-zero while any obligation is unverified")))

  (test-case "the required-offline lane is the suite union the doc names"
    (define lane (qt-lane-ref "required-offline"))
    (check-equal? (qt-lane-suites lane) required-offline-suites)
    (check-equal? required-offline-suites '(fast security workflows smoke platform))
    (check-true (> (length (qt-lane-suites lane)) 1)
                "a suite union: the CI surface refuses it, so the doc must say so")
    (check-true (string-contains? flat "refuses on the CI surface"))
    (for ([producer (in-list required-ci-producers)])
      (check-not-false (qt-lane-ref (required-producer-lane producer))))
    (for ([suite (in-list required-offline-suites)])
      (check-true (string-contains? flat (format "`~a`" suite))
                  (format "the doc names suite ~a" suite))))

  (test-case "the named compatibility consumers still exist and use the adapter"
    (check-true (file-exists? compat-path) "scripts/run-tests.rkt is retained")
    (for ([rel (in-list '("scripts/pre-commit.rkt" "extensions/gsd/effective-verify-binding.rkt"
                                                   "extensions/gsd/evidence-runner-coverage.rkt"
                                                   "scripts/testing/changed-selection.rkt"))])
      (define p (build-path root rel))
      (check-true (file-exists? p) (format "~a exists" rel))
      (check-true (string-contains? (file->string p) "scripts/run-tests.rkt")
                  (format "~a still uses scripts/run-tests.rkt" rel))
      (check-true (string-contains? flat rel) (format "the doc names ~a" rel)))
    ;; every legacy flag the budget table quotes is still implemented by the
    ;; compat grammar (run-tests.rkt delegates to runner-cli.rkt)
    (for ([flag '("--changed-base" "--changed-head" "--impact-dry-run" "--prioritize" "--suite")])
      (check-true (string-contains? compat-text flag) (format "legacy flag ~a implemented" flag)))
    (check-true (string-contains? (file->string (build-path root
                                                            "scripts/testing/changed-selection.rkt"))
                                  "--generate-covers-manifest")))

  (test-case "the admission audit and preparation claims match the implementation"
    (check-true (file-exists? admission-record) "tests/.quick-admission.rktd is committed")
    (check-true (string-contains? flat "tests/.quick-admission.rktd"))
    (define audit-text (file->string quick-audit-path))
    (check-true (string-contains? audit-text "quick admission audit"))
    (for ([state '("admitted" "review-required" "rejected")])
      (check-true (string-contains? audit-text state)))
    (define prep-text (file->string preparation-path))
    (check-true (string-contains? prep-text "Q_SKIP_PREPARATION"))
    (check-true (string-contains? prep-text "substantial-work threshold"))
    (check-true (string-contains? flat "Q_SKIP_PREPARATION=1"))
    (check-true (string-contains? flat "substantial-work threshold"))
    (define runner-text (file->string runner-path))
    (check-true (string-contains? runner-text "preparation ok"))
    (check-true (string-contains? runner-text "targets="))
    (check-true (string-contains? runner-text "identity="))
    (check-true (string-contains? flat "hash → make → re-hash"))))

;; Phase D (testing acceleration, #9936): pins for the everyday four-command
;; loop, its measured-cost honesty, the no-owner gap path, and the admission
;; expansion status. Same contract as above: a doc claim that drifts from the
;; code is a red test; the doc moves in the same commit as its contract.
(module+ test
  (test-case "the everyday four-command loop and its measured costs are documented"
    (check-true (string-contains? flat "The documented day is four commands"))
    (for ([tok '("`./qt quick <file>`" "`./qt quick --changed BASE..HEAD`"
                                       "`./qt full --file <file>`"
                                       "`./qt default --file <file>`"
                                       "`./qt default`")])
      (check-true (string-contains? doc tok) (format "the everyday loop names ~a" tok)))
    (check-true (string-contains? qt-text "default does not accept CI-surface option"))
    (check-true (string-contains? flat "20 samples"))
    (check-true (string-contains? flat "5 samples"))
    (check-equal? (length (regexp-match* #rx"[*][*]missed" doc))
                  2
                  "both loop-target misses are recorded, not hidden"))

  (test-case "the no-owner gap path matches the implemented escalation behavior"
    (check-true (string-contains? flat "never answers a gap with a zero-test green"))
    (define cs-text (file->string changed-selection-path))
    (check-true (string-contains? cs-text "no @covers mapping reaches this changed module"))
    (check-true (string-contains? flat "no @covers mapping reaches this changed module"))
    (check-true (string-contains? cs-text "fallback-suites"))
    (check-true (string-contains? flat "fallback_suites"))
    (check-true (string-contains? qt-text "changed selection is empty"))
    (check-true (string-contains? flat "changed selection is empty")))

  (test-case "the admission expansion status names the real B1-B3 contract owners"
    (check-true (string-contains? flat "no admission expansion happened"))
    (for ([f (in-list '("tests/test-gsd-auto-review-loop-contract.rkt"
                        "tests/test-gsd-auto-review-production-contract.rkt"
                        "tests/test-gsd-generated-metadata-correction-contract.rkt"))])
      (check-true (file-exists? (build-path root f)) (format "~a exists" f))
      (check-true (string-contains? flat f) (format "the doc names ~a" f)))
    (for ([f (in-list '("tests/test-gsd-auto-review-loop-contract.rkt"
                        "tests/test-gsd-auto-review-production-contract.rkt"
                        "tests/test-gsd-generated-metadata-correction-contract.rkt"))])
      (check-false (member f (admitted-owners)) (format "~a stays unadmitted" f)))))
