#lang racket/base

;; @speed slow
;; @suite testing
;; @isolation process
;; @boundary integration  ;; @mutates fs
;; W8: end-to-end quarantine-expiry + status-distinction verification through
;; the real runner CLI and the W0 JSON report schema. Wave-8 acceptance:
;;   1. An expired known-failure quarantine (ledger `expires_on` past) surfaces
;;      as an ESCALATING FAILURE in report output — never a tolerated pass.
;;   2. An unexpired quarantine is still evidence-tagged (known_failure) and
;;      the file still reports status "fail" (definitive, never success).
;;   3. A timed-out file reports status "timeout" as a distinct status with a
;;      distinct exit code (2) and verdict "incomplete" — never success.
;;   4. Every file record carries execution_profile + runner_mode (W8) and the
;;      report carries the shard identity (W0 schema).
;;
;; Isolation: fixtures + ledgers live in a per-test temporary directory; the
;; runner spawns fresh `racket scripts/run-tests.rkt` processes (startup does
;; repo-wide stale-bytecode cleaning — documented shared-tree surface, same
;; retention rationale as tests/test-run-tests-ledger.rkt).

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define scratch-dir (make-temporary-file "w8-quarantine-e2e-~a" 'directory))

(define (scratch-path . parts) (apply build-path scratch-dir parts))

(define (write-text! path lines)
  (call-with-output-file* path
                          #:exists 'truncate/replace
                          (lambda (out) (for ([l (in-list lines)]) (displayln l out))))
  path)

;; Deterministic failing fixture: an unresolvable module path classifies as
;; MODULE_LOAD_FAILURE (same trick as the W2 CLI ledger test).
(define fail-fixture
  (write-text! (scratch-path "w8-fail-fixture.rkt")
               '("#lang racket/base"
                 "(require definitely/missing/w8/quarantine/module)")))

;; Deterministic slow fixture: exceeds --timeout, classifies as TIMEOUT.
(define timeout-fixture
  (write-text! (scratch-path "w8-timeout-fixture.rkt")
               '("#lang racket/base"
                 "(sleep 30)")))

(define (quarantine-entry-for fixture expires-on)
  (hasheq 'file (path->string fixture)
          'category "MODULE_LOAD_FAILURE"
          'owner "w8-verify"
          'first_seen "0.99.28"
          'release_blocking #t
          'issue "#9502"
          'notes "W8 e2e quarantine fixture (scratch, never committed)"
          'expires_on expires-on))

(define (write-ledger! entries name)
  (define path (scratch-path name))
  (call-with-output-file* path
                          #:exists 'truncate/replace
                          (lambda (out) (write-json (hasheq 'version 1 'entries entries) out)))
  path)

;; Run the real CLI on an explicit fixture and read back the W0-schema JSON
;; report. Returns (exit-code report-jsexpr).
(define (run-report! ledger fixture #:timeout-secs [timeout-secs 60] #:tag tag)
  (define report-path (scratch-path (format "report-~a.json" tag)))
  (define cmd
    (string-join
     (list "racket"
           (path->string (build-path project-root "scripts" "run-tests.rkt"))
           "--sequential"
           "--mode subprocess"
           "--profile local"
           (format "--timeout ~a" timeout-secs)
           (format "--ledger ~a" (path->string ledger))
           (format "--json-out ~a" (path->string report-path))
           "--shard-index 0 --shard-total 2"
           (path->string fixture))))
  (define code
    (parameterize ([current-directory project-root]
                   [current-output-port (open-output-string)]
                   [current-error-port (open-output-string)])
      (system/exit-code cmd)))
  (values code (with-input-from-file report-path read-json)))

(define (file-record report fixture)
  (define target (path->string fixture))
  (for/first ([f (in-list (hash-ref report 'files '()))]
              #:when (string=? (hash-ref f 'path #f) target))
    f))

(define suite
  (test-suite
   "W8 full-regression quarantine expiry + distinct statuses (e2e)"

   (test-case "expired quarantine escalates: report output flags the failure"
     (define ledger
       (write-ledger! (list (quarantine-entry-for fail-fixture "2020-01-01"))
                      "ledger-expired.json"))
     (define-values (code report) (run-report! ledger fail-fixture #:tag "expired"))
     ;; Definitive non-success, non-timeout exit.
     (check-equal? code 1)
     (check-equal? (hash-ref report 'verdict) "fail")
     ;; Ledger summary counts: expired quarantines are their own bucket.
     (define counts (hash-ref report 'ledger))
     (check-equal? (hash-ref counts 'expired_quarantine_failures) 1)
     (check-equal? (hash-ref counts 'known_failures) 0)
     ;; Per-file evidence: escalating failure with expiry surfaced.
     (define f (file-record report fail-fixture))
     (check-true (hash-ref f 'known_failure))
     (check-true (hash-ref f 'quarantine_expired))
     (check-true (hash-ref f 'escalate))
     (check-equal? (hash-ref f 'expires_on) "2020-01-01")
     (check-equal? (hash-ref f 'status) "fail")
     (check-equal? (hash-ref f 'issue) "#9502")
     (check-equal? (hash-ref f 'release_blocking) #t)
     ;; W8: profile + runner mode ride on every file record.
     (check-equal? (hash-ref f 'execution_profile) "local")
     (check-equal? (hash-ref f 'runner_mode) "subprocess")
     ;; W0/W8: run summary keeps statuses distinct and records the shard.
     (define rs (hash-ref report 'run_summary))
     (check-equal? (hash-ref rs 'fail) 1)
     (check-equal? (hash-ref rs 'timeout) 0)
     (check-equal? (hash-ref rs 'pass) 0)
     (check-equal? (hash-ref (hash-ref rs 'shard) 'index) 0))

   (test-case "unexpired quarantine is tolerated but still fails definitively"
     (define ledger
       (write-ledger! (list (quarantine-entry-for fail-fixture "2999-12-31"))
                      "ledger-active.json"))
     (define-values (code report) (run-report! ledger fail-fixture #:tag "active"))
     (check-equal? code 1)
     (check-equal? (hash-ref report 'verdict) "fail")
     (define counts (hash-ref report 'ledger))
     (check-equal? (hash-ref counts 'known_failures) 1)
     (check-equal? (hash-ref counts 'expired_quarantine_failures) 0)
     ;; Tolerated known failure — evidence-tagged, not escalated.
     (define f (file-record report fail-fixture))
     (check-true (hash-ref f 'known_failure))
     (check-false (hash-ref f 'escalate #f))
     (check-false (hash-ref f 'quarantine_expired #f))
     (check-equal? (hash-ref f 'status) "fail"))

   (test-case "timeout is a distinct status — never reported as success"
     (define ledger
       (write-ledger! '() "ledger-empty.json"))
     (define-values (code report)
       (run-report! ledger timeout-fixture #:timeout-secs 5 #:tag "timeout"))
     (check-equal? code 2)
     (check-equal? (hash-ref report 'verdict) "incomplete")
     (define f (file-record report timeout-fixture))
     (check-equal? (hash-ref f 'status) "timeout")
     (check-false (hash-ref f 'known_failure #f))
     (define rs (hash-ref report 'run_summary))
     (check-equal? (hash-ref rs 'timeout) 1)
     (check-equal? (hash-ref rs 'fail) 0)
     (check-equal? (hash-ref rs 'pass) 0))))

(module+ main
  (define failed? (run-tests suite))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files scratch-dir))
  (unless (zero? failed?) (exit failed?)))

(module+ test
  (require (prefix-in rackunit: rackunit/text-ui))
  (define failed? (rackunit:run-tests suite))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files scratch-dir))
  (unless (zero? failed?) (exit failed?)))
