#lang racket

;; @speed slow
;; @suite default
;; @boundary unit

;; BOUNDARY: integration

;; tests/test-audit-script.rkt — Tests for scripts/audit-project.rkt (RA-4)

(require rackunit
         "../util/version.rkt"
         rackunit/text-ui
         racket/string
         racket/file
         racket/path
         racket/port
         racket/runtime-path)

;; BUG-0033: resolve q-dir relative to THIS FILE so the test passes from
;; any cwd (was (current-directory)-based).
(define-runtime-path q-dir "..")

(define audit-script (build-path q-dir "scripts" "audit-project.rkt"))

(define (run-audit-args . args)
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (apply subprocess #f #f #f (find-executable-path "racket") (path->string audit-script) args)))
  (define output (port->string out))
  (define stderr-output (port->string err))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  (subprocess-wait sp)
  (define exit-code (subprocess-status sp))
  (values output stderr-output exit-code))

(define (run-audit-stdout)
  (define-values (output stderr exit-code) (run-audit-args "--stdout"))
  output)

(define (run-audit-json)
  (define-values (output stderr exit-code) (run-audit-args "--json"))
  output)

;; RA-4 W1: real audit CWD-safety ownership lives HERE (slow/L4), not in
;; the fast test-cwd-independence.rkt. The fast test owns only the minimal
;; probe; this helper runs the absolute audit script with the launcher's
;; cwd parked in an isolated system temp directory (arbitrary CWD). The
;; audit subprocess itself gets the repo root as cwd — production contract:
;; Q-DIR resolves from the audit process's cwd and must NOT be changed
;; here. No nested suite execution and no duplication of the full
;; assertion set above — just exit status and the key output contract.
(define (run-audit-with-launcher-cwd cwd . args)
  ;; Launcher contract: the CALLER parks in an arbitrary cwd (`cwd`), then
  ;; invokes the audit by absolute path. Racket subprocesses inherit the
  ;; parameterized current-directory, and the audit's production contract
  ;; (scripts/audit-project.rkt Q-DIR) resolves the repo root from the audit
  ;; process's own cwd — so the CHILD is parameterized to q-dir while the
  ;; launcher sits at `cwd`. This mirrors CI invocations by absolute path
  ;; with the repo root as the command's working directory.
  (define caller-cwd (current-directory))
  (current-directory cwd)
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (apply subprocess #f #f #f (find-executable-path "racket")
             (path->string audit-script) args)))
  (current-directory caller-cwd)
  (define output (port->string out))
  (define stderr-output (port->string err))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  (subprocess-wait sp)
  (values output stderr-output (subprocess-status sp)))

;; Audit subprocess with an arbitrary cwd (only safe for cwd-independent
;; flags like --help, which print usage and exit before any crawl).
(define (run-audit-from-arbitrary-cwd . args)
  (apply run-audit-with-launcher-cwd (find-system-path 'temp-dir) args))

(define audit-tests
  (test-suite "Audit Script Tests"

    (test-case "audit script exists and is readable"
      (check-true (file-exists? audit-script)))

    (test-case "audit --stdout produces valid markdown report"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "# Q Project Audit Report"))
      (check-true (string-contains? output "## Module Inventory"))
      (check-true (string-contains? output "## Findings"))
      (check-true (string-contains? output "## Summary")))

    (test-case "audit report contains module count"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "Total modules"))
      (check-true (regexp-match? #rx"Total modules.*: [0-9]+" output)))

    (test-case "audit report includes layer breakdown"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "runtime"))
      (check-true (string-contains? output "tests")))

    ;; --- T1: Expanded tests ---

    (test-case "audit --help prints usage and exits 0"
      (define-values (out err exit-code) (run-audit-args "--help"))
      (check-true (string-contains? out "Usage:"))
      (check-true (string-contains? out "--stdout"))
      (check-true (string-contains? out "--ci"))
      (check-true (string-contains? out "json"))
      (check-equal? exit-code 0))

    (test-case "audit -h prints usage and exits 0"
      (define-values (out err exit-code) (run-audit-args "-h"))
      (check-true (string-contains? out "Usage:"))
      (check-equal? exit-code 0))

    (test-case "json mode produces output with expected keys"
      (define output (run-audit-json))
      (check-true (string-contains? output "timestamp"))
      (check-true (string-contains? output "version"))
      (check-true (string-contains? output "modules"))
      (check-true (string-contains? output "risky_apis"))
      (check-true (string-contains? output "todos"))
      (check-true (string-contains? output "critical_count")))

    (test-case "json output version matches current version"
      (define output (run-audit-json))
      (define m (regexp-match #rx"version.*?([0-9]+\\.[0-9]+\\.[0-9]+)" output))
      (check-not-false m)
      (when m
        (check-true (string=? (cadr m) q-version))))

    (test-case "ci mode exits 0 when no critical findings"
      (define-values (out err exit-code) (run-audit-args "--ci"))
      (check-equal? exit-code 0))

    (test-case "stdout has no self-referential script findings"
      (define output (run-audit-stdout))
      (check-false (string-contains? output "scripts/audit-project.rkt")))

    (test-case "unknown flag prints warning on stderr"
      (define-values (out err exit-code) (run-audit-args "--bogus"))
      (check-true (string-contains? err "Warning: unknown flag --bogus")))

    (test-case "json module count is positive"
      (define output (run-audit-json))
      (define m (regexp-match #rx"total.*?([0-9]+)" output))
      (check-not-false m)
      (when m
        (check-true (> (string->number (cadr m)) 0))))

    ;; --- RA-4 W1: audit is invocable from an arbitrary cwd (real behavior) ---

    (test-case "RA-4 W1: audit --stdout real contract with launcher cwd at system temp dir"
      (define-values (out err exit-code)
        (run-audit-with-launcher-cwd (find-system-path 'temp-dir) "--stdout"))
      (check-equal? exit-code 0 "audit must exit 0 with the launcher parked in a temp dir")
      (check-true (string-contains? out "# Q Project Audit Report"))
      (check-true (regexp-match? #rx"Total modules.*: [0-9]+" out)))

    (test-case "RA-4 W1: audit --help is CWD-safe from an arbitrary cwd"
      (define-values (out err exit-code) (run-audit-from-arbitrary-cwd "--help"))
      (check-equal? exit-code 0)
      (check-true (string-contains? out "Usage:")))))

(run-tests audit-tests)
