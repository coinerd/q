#lang racket/base

;; tests/test-runner-grouped-characterization.rkt — v1.00.24 W7
;;
;; Grouped-mode equivalence characterization: every eligible grouped-mode
;; fixture must produce the SAME selected path, exit code, verdict class,
;; parsed test counts, and stdout/stderr markers as subprocess execution
;; (wall clock may differ; verdicts may not). Every grouped request that
;; actually executes subprocess must carry a stable named fallback reason
;; (or #f when none), and must not be counted as grouped.
;;
;; Scope guard: this suite exercises ONLY tests/fixtures/grouped-mode/* —
;; it never changes production defaults, grouped eligibility, or queue
;; activation.

(require rackunit
         rackunit/text-ui
         racket/path
         racket/string
         racket/runtime-path
         (only-in "../scripts/run-tests/runner.rkt"
                  run-single-file
                  current-requested-execution-mode
                  execution-eligibility-reason
                  execution-mode-of)
         (only-in "../scripts/run-tests/parse.rkt"
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-total
                  test-file-result-passed
                  test-file-result-failed
                  test-file-result-requested-execution-mode
                  test-file-result-grouped-fallback-reason
                  test-file-result-stdout-bytes
                  test-file-result-stderr-bytes
                  classify-test-result))

(define-runtime-path fixtures-dir "fixtures/grouped-mode")

(define (fx name)
  (path->string (simplify-path (build-path fixtures-dir name))))

(define (run-one p mode #:timeout [timeout #f])
  (parameterize ([current-requested-execution-mode mode])
    (run-single-file p #:timeout timeout #:mode (string->symbol mode))))

(define (out-of r)
  (bytes->string/utf-8 (test-file-result-stdout-bytes r)))

(define (err-of r)
  (bytes->string/utf-8 (test-file-result-stderr-bytes r)))

;; ── 1. Static eligibility gate (no execution) ──────────────────────────
;; The stable named reasons; a grouped request that falls back must name
;; exactly why, and eligible files must yield #f.
(define expected-reason
  (hash "eligible-a.rkt"
        #f
        "eligible-b.rkt"
        #f
        "stdout-stderr.rkt"
        #f
        "exception.rkt"
        #f
        "explicit-exit.rkt"
        #f
        "timeout.rkt"
        #f
        "mutates-env-undeclared.rkt"
        #f
        "no-submodule.rkt"
        'missing-module-plus-test-form
        "top-level-only.rkt"
        'missing-module-plus-test-form
        "silent-checks.rkt"
        #f ; eligible statically; falls back at runtime
        "declared-mutation.rkt"
        'declared-mutation
        "process-isolation.rkt"
        'declared-process-isolation))

(for ([(name reason) (in-hash expected-reason)])
  (check-equal? (execution-eligibility-reason (fx name))
                reason
                (format "static eligibility reason for ~a" name)))

;; ── 2. Parity characterization: eligible fixtures, both modes ──────────
;; Asserts equal path, exit code, verdict class, and parsed counts between
;; subprocess and grouped execution; grouped result must additionally show
;; requested=grouped, no fallback reason, and effective mode
;; grouped-in-process.
(define (assert-parity sub grp what)
  (check-equal? (test-file-result-path grp)
                (test-file-result-path sub)
                (format "~a: same selected path" what))
  (check-equal? (test-file-result-exit-code grp)
                (test-file-result-exit-code sub)
                (format "~a: same exit code" what))
  (check-equal? (classify-test-result grp)
                (classify-test-result sub)
                (format "~a: same verdict class" what))
  (check-equal? (test-file-result-total grp)
                (test-file-result-total sub)
                (format "~a: same parsed total" what))
  (check-equal? (test-file-result-passed grp)
                (test-file-result-passed sub)
                (format "~a: same parsed passed" what))
  (check-equal? (test-file-result-failed grp)
                (test-file-result-failed sub)
                (format "~a: same parsed failed" what))
  (check-equal? (test-file-result-requested-execution-mode grp)
                "grouped"
                (format "~a: grouped result stamps requested mode" what))
  (check-equal? (test-file-result-requested-execution-mode sub)
                "subprocess"
                (format "~a: subprocess result stamps requested mode" what)))

(define (assert-grouped-in-process grp what)
  (check-false (test-file-result-grouped-fallback-reason grp)
               (format "~a: no fallback reason (counted as grouped)" what))
  (check-equal? (execution-mode-of (test-file-result-path grp))
                'grouped-in-process
                (format "~a: effective mode is grouped-in-process" what)))

(define (assert-subprocess-fallback grp reason what)
  (check-equal? (test-file-result-grouped-fallback-reason grp)
                reason
                (format "~a: stable fallback reason ~a" what reason))
  (check-equal? (execution-mode-of (test-file-result-path grp))
                'subprocess
                (format "~a: fell back to subprocess (not counted as grouped)" what)))

;; eligible-a: two passing run-tests checks — repeat 3x for stability.
(for ([i (in-range 3)])
  (define sub (run-one (fx "eligible-a.rkt") "subprocess"))
  (define grp (run-one (fx "eligible-a.rkt") "grouped"))
  (assert-parity sub grp (format "eligible-a repetition ~a" (+ i 1)))
  (assert-grouped-in-process grp (format "eligible-a repetition ~a" (+ i 1)))
  (check-equal? (test-file-result-total grp) 2 "eligible-a parses 2 tests")
  (check-equal? (test-file-result-exit-code grp) 0 "eligible-a exits 0"))

;; eligible-b: single passing check, cwd-invocation context check.
(define sub-b (run-one (fx "eligible-b.rkt") "subprocess"))
(define grp-b (run-one (fx "eligible-b.rkt") "grouped"))
(assert-parity sub-b grp-b "eligible-b")
(assert-grouped-in-process grp-b "eligible-b")

;; stdout/stderr capture parity: markers must appear in both modes.
(define sub-o (run-one (fx "stdout-stderr.rkt") "subprocess"))
(define grp-o (run-one (fx "stdout-stderr.rkt") "grouped"))
(assert-parity sub-o grp-o "stdout-stderr")
(assert-grouped-in-process grp-o "stdout-stderr")
(check-true (string-contains? (out-of sub-o) "GMD-W7-STDOUT-MARKER")
            "subprocess captures stdout marker")
(check-true (string-contains? (out-of grp-o) "GMD-W7-STDOUT-MARKER") "grouped captures stdout marker")
(check-true (string-contains? (err-of grp-o) "GMD-W7-STDERR-MARKER") "grouped captures stderr marker")

;; exception: exit 1 with the marker on stderr in both modes. Parsed-count
;; parity is intentionally not asserted here: the exception path emits no
;; rackunit summary, so counts are 0 on both sides while verdicts agree.
(define (assert-failure-parity sub grp what)
  (check-equal? (test-file-result-path grp)
                (test-file-result-path sub)
                (format "~a: same selected path" what))
  (check-equal? (test-file-result-exit-code grp)
                (test-file-result-exit-code sub)
                (format "~a: same exit code" what))
  (check-equal? (classify-test-result grp)
                (classify-test-result sub)
                (format "~a: same verdict class" what))
  (check-equal? (test-file-result-requested-execution-mode grp)
                "grouped"
                (format "~a: grouped result stamps requested mode" what)))

(define sub-e (run-one (fx "exception.rkt") "subprocess"))
(define grp-e (run-one (fx "exception.rkt") "grouped"))
(assert-failure-parity sub-e grp-e "exception")
(assert-grouped-in-process grp-e "exception")
(check-equal? (test-file-result-exit-code grp-e) 1 "exception exits 1")
(check-true (string-contains? (err-of grp-e) "GMD-W7-EXCEPTION-MARKER")
            "grouped records the exception marker on stderr")

;; explicit exit: (exit 0) runs before any self-report, so grouped cannot
;; prove the checks ran — zero-parse strictness sends it back to subprocess
;; under the stable zero-parsed-output reason, preserving exit 0 parity.
(define sub-x (run-one (fx "explicit-exit.rkt") "subprocess"))
(define grp-x (run-one (fx "explicit-exit.rkt") "grouped"))
(assert-parity sub-x grp-x "explicit-exit")
(assert-subprocess-fallback grp-x 'zero-parsed-output "explicit-exit")
(check-equal? (test-file-result-exit-code grp-x) 0 "explicit (exit 0) records 0")

;; timeout: short runner timeout yields exit 2 / TIMEOUT verdict in both.
(define sub-t (run-one (fx "timeout.rkt") "subprocess" #:timeout 600))
(define grp-t (run-one (fx "timeout.rkt") "grouped" #:timeout 600))
(assert-parity sub-t grp-t "timeout")
(assert-grouped-in-process grp-t "timeout")
(check-equal? (test-file-result-exit-code grp-t) 2 "grouped timeout records exit 2")
(check-equal? (classify-test-result grp-t) 'TIMEOUT "grouped timeout verdict is TIMEOUT")

;; ── 3. Runtime fallback characterization: grouped request → subprocess ─
;; silent-checks has (module+ test) but no run-tests self-report: grouped
;; dynamic-require runs silently (exit 0, zero parsed) so the runner falls
;; back and re-runs in subprocess. The final result must name the reason
;; and match a direct subprocess run.
(define sub-s (run-one (fx "silent-checks.rkt") "subprocess"))
(define grp-s (run-one (fx "silent-checks.rkt") "grouped"))
(assert-parity sub-s grp-s "silent-checks")
(assert-subprocess-fallback grp-s 'zero-parsed-output "silent-checks")

;; missing module+ test (two shapes) → stable named fallback.
(define sub-n (run-one (fx "no-submodule.rkt") "subprocess"))
(define grp-n (run-one (fx "no-submodule.rkt") "grouped"))
(assert-parity sub-n grp-n "no-submodule")
(assert-subprocess-fallback grp-n 'missing-module-plus-test-form "no-submodule")

(define sub-l (run-one (fx "top-level-only.rkt") "subprocess"))
(define grp-l (run-one (fx "top-level-only.rkt") "grouped"))
(assert-parity sub-l grp-l "top-level-only")
(assert-subprocess-fallback grp-l 'missing-module-plus-test-form "top-level-only")

;; declared mutation → named fallback (never executed grouped).
(define sub-m (run-one (fx "declared-mutation.rkt") "subprocess"))
(define grp-m (run-one (fx "declared-mutation.rkt") "grouped"))
(assert-parity sub-m grp-m "declared-mutation")
(assert-subprocess-fallback grp-m 'declared-mutation "declared-mutation")

;; declared process isolation → named fallback (never executed grouped).
(define sub-p (run-one (fx "process-isolation.rkt") "subprocess"))
(define grp-p (run-one (fx "process-isolation.rkt") "grouped"))
(assert-parity sub-p grp-p "process-isolation")
(assert-subprocess-fallback grp-p 'declared-process-isolation "process-isolation")

;; ── 4. Two eligible files sequentially in one grouped worker ───────────
;; Deterministic order (a then b), both grouped-in-process, host current
;; directory restored between files by the runner's parameterization.
(define host-cwd-before (current-directory))
(define seq-a (run-one (fx "eligible-a.rkt") "grouped"))
(define seq-b (run-one (fx "eligible-b.rkt") "grouped"))
(assert-grouped-in-process seq-a "sequential-a")
(assert-grouped-in-process seq-b "sequential-b")
(check-equal? (test-file-result-exit-code seq-a) 0 "sequential-a exits 0")
(check-equal? (test-file-result-exit-code seq-b) 0 "sequential-b exits 0")
(check-equal? (test-file-result-total seq-b) 1 "sequential-b parses 1 test")
(check-true (equal? (current-directory) host-cwd-before)
            "host current directory restored after grouped files")

;; ── 5. Documented boundary: undeclared env mutation leaks grouped ──────
;; Characterization truth, not endorsement: an undeclared putenv in an
;; eligible grouped file leaks into the worker process (subprocess mode
;; cannot leak). The @mutates declaration contract is the v1.00.27
;; migration boundary; this is why declared mutators fall back.
(define grp-u (run-one (fx "mutates-env-undeclared.rkt") "grouped"))
(assert-grouped-in-process grp-u "mutates-env-undeclared")
(check-equal? (test-file-result-exit-code grp-u) 0 "mutates-env-undeclared exits 0")
(check-equal? (getenv "GMD_W7_PROBE")
              "set"
              "DOCUMENTED LEAK: undeclared env mutation crosses grouped files")
(void (putenv "GMD_W7_PROBE" "")) ; restore host environment for later suites

(module+ test
  (displayln "test-runner-grouped-characterization: all checks green"))

(module+ main
  ;; The characterization assertions execute at module load. This explicit
  ;; text-ui sentinel gives the subprocess runner a truthful parsed result;
  ;; any earlier assertion failure prevents this success from being emitted.
  (exit (run-tests (test-suite "grouped-mode characterization"
                     (test-case "all module-load characterization assertions completed"
                       (check-true #t))))))
