#lang racket

;; @speed fast
;; @suite testing
;; @boundary integration
;; @covers scripts/testing/preparation.rkt

;; tests/test-run-tests-preparation.rkt — Wave C build-once preparation
;; (final plan §4 rev 3). Covers the §9 correctness cases owned by this
;; workstream: stable/changed identity, exclusion-file hygiene (existence +
;; negative-compilation), unlisted-failing-target hard error, abort on
;; identity mismatch, Q_SKIP_PREPARATION escape hatch, and telemetry fields.
;; Fixture trees live in temp dirs; the only repo files touched are READS
;; (the negative-compilation guard compiles the four listed fixtures in
;; place — they fail fast, well under a second each, and write nothing).

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         (only-in file/sha1 sha1)
         "../scripts/testing/preparation.rkt"
         "../scripts/run-tests/reporting.rkt"
         "../scripts/run-tests/runner.rkt")

;; Anchored to THIS FILE's location (define-runtime-path), not run-file:
;; the runner spawns children as `raco test -t <file>`, where run-file is
;; raco's own path (see test-runner-base-dir-resolution.rkt).
(define-runtime-path repo-root "..")

;; ── fixture-tree helper ─────────────────────────────────────────────────

(define (make-fixture-tree!)
  (define root (make-temporary-file "prep-test-~a" 'directory))
  (define (w rel text)
    (define p (build-path root rel))
    (make-directory* (path-only p))
    (display-to-file text p #:exists 'replace))
  (w "app/helper.rkt" "#lang racket/base\n(provide helper)\n(define (helper) 41)\n")
  (w "app/dep/bad.rkt" "this is not a module\n")
  ;; bad.rkt is reachable by extract-requires (the require spec sits inside
  ;; quoted DATA, which the extractor walks) but is compiled by nothing —
  ;; the mechanical mirror of the real copy-input fixtures.
  (w "tests/test-main.rkt"
     "#lang racket/base\n(require \"../app/helper.rkt\")\n(define quoted-specs '(require \"../app/dep/bad.rkt\"))\n(displayln 'noop)\n")
  root)

;; ── the mismatch re-check from prepare-files!, as a testable seam ───────
;; (the same per-path comparison prepare-files! performs between its pre-make
;; identity and its immediate re-hash; final plan §9 "source edited between
;; preparation and the first dispatch" — the abort must name the path)

(define (mismatched-paths root before)
  (filter
   string?
   (for/list ([entry (in-list before)])
     (define rel (car entry))
     (define after
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (call-with-input-file (build-path root rel) sha1)))
     (and (not (equal? after (cdr entry))) rel))))

(module+ test
  (define (prepare-for-test! root files #:exclusions [exclusions #f])
    ;; the threshold gate exists for §4.4 substantial-work routing; tests
    ;; force the prepared path on their tiny fixture trees
    (parameterize ([preparation-min-closure 0])
      (prepare-files! root files #:exclusions exclusions)))

  (test-case "identity is stable across runs and changes when content changes"
    (define root (make-fixture-tree!))
    (define paths (list "app/helper.rkt" "tests/test-main.rkt"))
    (define id1 (preparation-identity-hash root paths))
    (define id2 (preparation-identity-hash root paths))
    (check-equal? id1 id2 "same content must give the same identity")
    (check-true (string? id1))
    ;; change one file's content → different identity
    (display-to-file "#lang racket/base\n(provide helper)\n(define (helper) 42)\n"
                     (build-path root "app/helper.rkt") #:exists 'replace)
    (define id3 (preparation-identity-hash root paths))
    (check-not-equal? id1 id3 "content change must change the identity")
    ;; path-set change also changes the identity
    (define id4 (preparation-identity-hash root (list "tests/test-main.rkt")))
    (check-not-equal? id1 id4 "path-set change must change the identity")
    (delete-directory/files root))

  (test-case "unlisted failing target is a HARD error (no source-mode fallback)"
    (define root (make-fixture-tree!))
    ;; selecting the garbage file directly (an explicit --file reproduction)
    ;; must fail preparation, never fall back to source mode
    (define result (prepare-for-test! root (list "app/dep/bad.rkt")))
    (check-equal? (preparation-result-outcome result) 'failed)
    (check-true (string-contains? (or (preparation-result-reason result) "")
                                  "raco make failed"))
    (delete-directory/files root))

  (test-case "listed fixture is skipped from make targets and preparation succeeds"
    (define root (make-fixture-tree!))
    ;; selecting main AND the fixture directly: the exclusion drops the
    ;; fixture from the make argv (it would otherwise be compiled and fail)
    (define result
      (prepare-for-test! root (list "tests/test-main.rkt" "app/dep/bad.rkt")
                         #:exclusions (list "app/dep/bad.rkt")))
    (check-equal? (preparation-result-outcome result) 'prepared)
    (check-equal? (preparation-result-excluded-count result) 1)
    (check-equal? (preparation-result-target-count result) 1)
    (check-equal? (preparation-result-closure-count result) 3)
    (check-true (string? (preparation-result-identity-prefix result)))
    (check-equal? (string-length (preparation-result-identity-prefix result)) 12)
    (check-equal? (preparation-result-reason result) #f)
    (delete-directory/files root))

  (test-case "identity mismatch aborts with the mismatched path named"
    (define root (make-fixture-tree!))
    (define paths (list "app/helper.rkt" "tests/test-main.rkt"))
    ;; pre-make identity, then mutate "during preparation"
    (define before
      (for/list ([rel (in-list paths)])
        (cons rel (call-with-input-file (build-path root rel) sha1))))
    (check-equal? (mismatched-paths root before) '()
                  "sanity: identical tree reports no mismatch")
    (display-to-file "#lang racket/base\n(provide helper)\n(define (helper) 7)\n"
                     (build-path root "app/helper.rkt") #:exists 'replace)
    (check-equal? (mismatched-paths root before) (list "app/helper.rkt")
                  "the mutated path must be named")
    ;; a vanished file is a mismatch too, not a crash
    (delete-file (build-path root "tests/test-main.rkt"))
    (check-equal? (mismatched-paths root before)
                  (list "app/helper.rkt" "tests/test-main.rkt")
                  "a vanished file is a mismatch (and both changes are named)")
    (delete-directory/files root))

  (test-case "Q_SKIP_PREPARATION skips everything (source-only legacy behavior)"
    (define root (make-fixture-tree!))
    (define saved (getenv "Q_SKIP_PREPARATION"))
    (dynamic-wind
        (lambda () (putenv "Q_SKIP_PREPARATION" "1"))
        (lambda ()
          (define result (prepare-for-test! root (list "tests/test-main.rkt")))
          (check-equal? (preparation-result-outcome result) 'skipped)
          (check-equal? (preparation-result-target-count result) 0)
          (check-false (preparation-result-identity-prefix result))
          ;; an UNLISTED failing target does NOT fail a skipped run
          (define result2 (prepare-for-test! root (list "app/dep/bad.rkt")))
          (check-equal? (preparation-result-outcome result2) 'skipped))
        (lambda ()
          (if saved
              (putenv "Q_SKIP_PREPARATION" saved)
              (putenv "Q_SKIP_PREPARATION" ""))
          (delete-directory/files root))))

  (test-case "closures below the substantial-work threshold stay source-only"
    (define root (make-fixture-tree!))
    (define result (prepare-files! root (list "tests/test-main.rkt")))
    (check-equal? (preparation-result-outcome result) 'skipped)
    (check-true (string-contains? (or (preparation-result-reason result) "")
                                  "threshold"))
    (delete-directory/files root)))

  (test-case "telemetry: prepared_environment merges preparation fields"
    (define prep
      (preparation-result 'prepared 1234 7 9 2 "abc123def456" #f '()))
    (define state (prepared-environment-state #:preparation prep))
    (check-equal? (hash-ref state 'preparation_outcome) "prepared")
    (check-equal? (hash-ref state 'preparation_elapsed_ms) 1234)
    (check-equal? (hash-ref state 'preparation_target_count) 7)
    (check-equal? (hash-ref state 'preparation_closure_count) 9)
    (check-equal? (hash-ref state 'preparation_excluded_count) 2)
    (check-equal? (hash-ref state 'preparation_identity_prefix) "abc123def456")
    ;; the W1 CI vocabulary is untouched by preparation telemetry
    (check-equal? (hash-ref state 'result) "unavailable")
    ;; the no-prep call keeps its W1 shape (keyword-compatible for old callers)
    (check-false (hash-has-key? (prepared-environment-state) 'preparation_outcome))
    ;; the merged block validates against the run-summary schema
    (define (summary-js pe)
      (hasheq 'run_summary
              (hasheq 'runner_version "test"
                      'suite "testing"
                      'profile "local"
                      'execution_mode "subprocess"
                      'file_count 0
                      'pass 0
                      'fail 0
                      'timeout 0
                      'skip 0
                      'wall_clock_seconds 0.0
                      'shard 'null
                      'metadata_completeness
                      (hasheq 'explicit 0 'heuristic 0 'missing 0))
              'files
              '()
              'extra
              (hasheq 'prepared_environment pe)))
    (check-equal? (validate-run-summary-json (summary-js state)) '())
    (check-true
     (ormap (lambda (p) (regexp-match? "preparation_outcome: expected" p))
            (validate-run-summary-json
             (summary-js (hash-set state 'preparation_outcome "invented"))))))

  (test-case "prepare-exclusions.rktd: non-empty; every path exists and is a negative-compilation fixture"
    (define exclusions (load-prepare-exclusions))
    (check-true (pair? exclusions) "the exclusions list must never be empty")
    (for ([entry (in-list exclusions)])
      (define rel (car entry))
      (define reason (cdr entry))
      (check-true (non-empty-string? reason)
                  (format "exclusion ~a must carry a reason" rel))
      (define path (build-path repo-root rel))
      (check-true (file-exists? path) (format "listed path must exist: ~a" rel))
      ;; negative-compilation check: `raco make` must FAIL on it in place.
      ;; The fixtures fail fast (<1s each) — the designed failure is the
      ;; missing sibling helper, and failing compiles write no .zo files.
      (define out (open-output-string))
      (define err (open-output-string))
      (define-values (sp stdin pid stderr ctrl)
        (apply values (process*/ports out #f err (find-executable-path "raco") "make" path)))
      (close-output-port stdin)
      (ctrl 'wait)
      (check-not-equal? (ctrl 'exit-code) 0
                        (format "listed fixture must fail compilation in place: ~a" rel))))
