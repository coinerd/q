#lang racket

;; @speed fast
;; @suite fast
;; @isolation offline
;; @boundary integration

;; W2 (v1.00.30) — eager compilation containment (issue #9688).
;;
;; The v1.00.29 regression: purge-plus-skipped-compile amplification. The
;; BUG-0065 purge (correctly) removes untrusted checkout bytecode on every
;; path, but the W3 prepared-environment restore SKIPS the package-visible
;; compile for restored shards — so the fast suite lazily re-compiles this
;; checkout inside every test shard, on every runner. Containment: the
;; coordinator pins the global rollback switch RACKET_PREPARED_ARTIFACT=off,
;; which must select the FULL path everywhere — purge untrusted checkout
;; bytecode, relink the current q, then one eager `raco setup --no-docs
;; --jobs 4 --pkgs q fmt` boundary per job. Containment is NOT a recovery
;; claim and NOT the final compiled-root architecture: subprocess mode, test
;; membership and worker counts stay identical.
;;
;; These tests are fixture-level (offline): they pin the workflow/action
;; wiring that makes the switch honest. The report-tool behavior for the
;; W2 telemetry fields and --containment-check lives in
;; tests/test-prepared-env-report.rkt.

(require rackunit
         racket/file
         racket/string)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define action-path (build-path project-root ".github" "actions" "setup-racket" "action.yml"))
(define ci-path (build-path project-root ".github" "workflows" "ci.yml"))

(define action-content (file->string action-path))
(define ci-content (file->string ci-path))

;; All PREPARED_ENV expressions: one per opting consumer call site. With
;; the global switch off, EVERY one of them must evaluate to 'off'.
(define prepared-env-expressions
  ;; Non-greedy: one expression per PREPARED_ENV line (Racket's `.` also
  ;; matches newlines, so a greedy `.*` would swallow the whole file).
  (for/list ([m (in-list (regexp-match* #rx"PREPARED_ENV: \\$\\{\\{ (.*?) \\}\\}"
                                        ci-content
                                        #:match-select cdr))])
    (car m)))

(define setup-call-sites
  (length (regexp-match* #rx"prepared-environment: \\$\\{\\{ env.PREPARED_ENV \\}\\}" ci-content)))

;; ---------------------------------------------------------------------------
;; Containment pinning: the global switch selects the full path everywhere
;; ---------------------------------------------------------------------------

(define-test-suite
 containment-pinning-tests
 (test-case "the fast-env producer is skipped by the global switch"
   (check-true (regexp-match? #rx"vars.RACKET_PREPARED_ARTIFACT != 'off'" ci-content)
               "fast-env and every consumer must honor RACKET_PREPARED_ARTIFACT=off"))
 (test-case "every opting consumer evaluates the global switch"
   (check-true (>= (length prepared-env-expressions) 4)
               "expected >=4 opting consumers (fast shards, report, release-dry-run, smoke)")
   (check-equal? (length prepared-env-expressions)
                 setup-call-sites
                 "every prepared-environment call site must resolve from a PREPARED_ENV expression")
   (for ([expr (in-list prepared-env-expressions)])
     (check-true (string-contains? expr "vars.RACKET_PREPARED_ARTIFACT != 'off'")
                 (format "consumer expression must honor the global switch: ~a" expr))))
 (test-case "a skipped or failed producer pins the full path (failure behavior)"
   (for ([expr (in-list prepared-env-expressions)])
     (check-true (string-contains? expr "needs.fast-env.result == 'success'")
                 (format "consumer expression must treat a non-success producer as off: ~a" expr))))
 (test-case "the workflow_dispatch override pins the full path"
   (for ([expr (in-list prepared-env-expressions)])
     (check-true (string-contains? expr "github.event_name != 'workflow_dispatch'")
                 (format "consumer expression must treat dispatch as off: ~a" expr)))))

;; ---------------------------------------------------------------------------
;; The full path IS the controlled eager compilation boundary
;; ---------------------------------------------------------------------------

(define eager-setup-line "raco setup --no-docs --jobs 4 --pkgs q fmt")

(define purge-step-block
  ;; From the purge step name to the next top-level step boundary.
  (car
   (regexp-match
    #px"- name: Purge and verify workspace bytecode \\(BUG-0065, every path\\).*?(?=- name:|- uses:)"
    action-content)))

(define-test-suite
 eager-boundary-tests
 (test-case "the full path purges untrusted checkout bytecode on every path"
   (check-true (and (string? purge-step-block) (not (equal? purge-step-block "")))
               "the BUG-0065 purge step must exist")
   (check-true (string-contains? purge-step-block "if: always()")
               "the purge must run on EVERY path (restore success included)"))
 (test-case "the full path relinks the current q checkout"
   (check-true (string-contains? action-content "raco pkg update --name q --link --batch --no-setup")
               "the full path must relink the current q before compiling"))
 (test-case "exactly one eager setup boundary per job"
   (check-equal? (length (regexp-match* (regexp-quote (string->bytes/utf-8 eager-setup-line))
                                        (string->bytes/utf-8 action-content)))
                 1
                 "the full path must keep exactly ONE eager raco setup boundary")
   (check-true (string-contains? action-content eager-setup-line)
               "the eager boundary must be raco setup --no-docs --jobs 4 --pkgs q fmt"))
 (test-case "no per-file compile fallback exists"
   (check-false (string-contains? action-content "raco make")
                "per-file raco make fallbacks are forbidden (one eager boundary per job)")))

;; ---------------------------------------------------------------------------
;; W2 telemetry: separate the store/restore outcome from the usable
;; compiled-code outcome; measure purge, eager compile, cache state
;; ---------------------------------------------------------------------------

(define-test-suite
 eager-telemetry-tests
 (test-case "the action exposes eager-compilation telemetry outputs"
   (for ([output (in-list '("eager-setup-seconds" "purge-zo-count" "cache-state"))])
     (check-true (string-contains? action-content (string-append "\n  " output ":"))
                 (format "action.yml must declare the ~a output at the action's top-level outputs"
                         output))))
 (test-case "ci.yml emits the W2 telemetry fields with every restore record"
   (for ([flag (in-list '("--compiled-code-outcome" "--eager-setup-seconds"
                                                    "--cache-state"
                                                    "--purge-zo-count"))])
     (check-true (string-contains? ci-content flag)
                 (format "the restore-record emit step must pass ~a" flag)))
   (check-true (string-contains? ci-content "steps.setup.outputs.eager-setup-seconds")
               "eager setup seconds must come from the action output")
   (check-true (string-contains? ci-content "steps.setup.outputs.purge-zo-count")
               "the purge count must come from the action output")
   (check-true (string-contains? ci-content "steps.setup.outputs.cache-state")
               "the cache state must come from the action output")
   (check-true (string-contains? ci-content "eager-full-path")
               "the compiled-code outcome must distinguish the eager full path")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (define failed 0)
  (set! failed (+ failed (run-tests containment-pinning-tests)))
  (set! failed (+ failed (run-tests eager-boundary-tests)))
  (set! failed (+ failed (run-tests eager-telemetry-tests)))
  (unless (zero? failed)
    (raise-user-error 'test-ci-eager-compilation "~a test failure(s)" failed)))

(module+ main
  (require rackunit/text-ui)
  (define failed 0)
  (set! failed (+ failed (run-tests containment-pinning-tests)))
  (set! failed (+ failed (run-tests eager-boundary-tests)))
  (set! failed (+ failed (run-tests eager-telemetry-tests)))
  (unless (zero? failed)
    (raise-user-error 'test-ci-eager-compilation "~a test failure(s)" failed)))
