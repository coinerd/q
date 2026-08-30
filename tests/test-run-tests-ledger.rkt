#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration  ;; @mutates fs
;; W2 PARALLEL-MODE FAILURE RECORD (fixed in W2)
;; ------------------------------------------------------------------
;; Repro:  cd q && racket scripts/run-tests.rkt --suite testing --jobs 3
;;         (any --jobs value; also reproducible file-by-file)
;; Standalone (before fix): racket tests/test-run-tests-ledger.rkt printed
;;   "2 success(es) 0 failure(s) 1 error(s) 3 test(s) run" but STILL exited 0,
;;   because a bare (run-tests suite) never sets the process exit code, which
;;   previously masked real errors behind exit 0. See the standalone exit
;;   record at the bottom of this file.

;;   reported FAILED by the runner while "passing" standalone.
;; Cause:  "parse-args accepts --ledger" destructured 14 values, but
;;   scripts/run-tests/cli.rkt parse-args returns 15 since --lint-metadata
;;   landed → "result arity mismatch; expected: 14, received: 15" ERROR.
;; Fix:    (1) bind the 15th value (_lint-metadata?);
;;         (2) exit non-zero from module+ main when rackunit reports any
;;             failure/error, so standalone runs fail loudly;
;;         (3) parse-args is exercised against a COPIED fixture ledger in a
;;             per-test temp file — the checked-in tests/test-suite-ledger.json
;;             is never read or mutated by this test.
;; Retained exemption: @isolation process (canonical; the `subprocess`
;;   alias is deprecated) is kept because the CLI test
;;   spawns `racket scripts/run-tests.rkt`, whose startup performs repo-wide
;;   stale-bytecode cleaning (see scripts/run-tests/classify-filters.rkt) —
;;   a genuine shared repository-tree surface that cannot be isolated inside
;;   this test. Documented in tests/helpers/README.md.

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define runner-module `(file ,(path->string (build-path project-root "scripts/run-tests.rkt"))))

(define load-known-failure-ledger*
  (dynamic-require runner-module
                   'load-known-failure-ledger
                   (lambda () (lambda (_) (error 'load-known-failure-ledger "missing export")))))

(define summarize-ledger-results*
  (dynamic-require runner-module
                   'summarize-ledger-results
                   (lambda () (lambda _ (error 'summarize-ledger-results "missing export")))))

;; W8 quarantine expiry surface (ledger schema `expires_on`).
(define ledger-summary-counts*
  (dynamic-require runner-module
                   'ledger-summary-counts
                   (lambda () (lambda _ (error 'ledger-summary-counts "missing export")))))

(define ledger-entry-expired?*
  (dynamic-require runner-module
                   'ledger-entry-expired?
                   (lambda () (lambda _ (error 'ledger-entry-expired? "missing export")))))

(define valid-expires-on?*
  (dynamic-require runner-module
                   'valid-expires-on?
                   (lambda () (lambda _ (error 'valid-expires-on? "missing export")))))

(define (result #:path path
                #:exit [exit-code 1]
                #:out [out "FAILURE"]
                #:failed [failed 1]
                #:total [total 1])
  (test-file-result path exit-code (string->bytes/utf-8 out) #"" 10 0 failed total))

(define (write-ledger entries)
  (define path (make-temporary-file "q-known-failures-~a.json"))
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (out) (write-json (hasheq 'version 1 'entries entries) out)))
  path)

(define (delete-file/safe path)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (file-exists? path)
      (delete-file path))))

(define (run/capture cmd)
  (parameterize ([current-directory project-root])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system/exit-code cmd)))
    (values code (get-output-string out) (get-output-string err))))

(define suite
  (test-suite "run-tests known-failure ledger"
    (test-case "parse-args accepts --ledger"
      ;; W2: operate on a copied fixture ledger in per-test scratch space,
      ;; never on the checked-in tests/test-suite-ledger.json.
      (define fixture (build-path project-root "tests/test-suite-ledger.json"))
      (define ledger-copy (make-temporary-file "q-ledger-fixture-~a.json"))
      (when (file-exists? fixture)
        (copy-file fixture ledger-copy #t))
      (dynamic-wind void
                    (lambda ()
                      (define-values (_jobs
                                      _seq?
                                      _timeout
                                      _strict?
                                      _suite
                                      _extra
                                      _repeat
                                      _record?
                                      _inventory?
                                      _diagnose?
                                      _mode
                                      _scheduler
                                      _json-out
                                      ledger
                                      _profile
                                      _lint-metadata?
                                      _changed-base
                                      _changed-head
                                      _explain?
                                      _impact-dry-run?
                                      _prioritize
                                      _failure-history
                                      _generate-covers-manifest?
                                      _shard-plan
                                      _durations)
                        (parse-args (list "--ledger" (path->string ledger-copy))))
                      (check-equal? ledger (path->string ledger-copy)))
                    (lambda () (delete-file/safe ledger-copy))))

    (test-case "known-failure ledger classifies known, new, unclassified, and resolved failures"
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    "tests/known.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9001"
                                    'notes
                                    "pre-existing assertion drift")
                            (hasheq 'file
                                    "tests/category-changed.rkt"
                                    'category
                                    "MODULE_LOAD_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #t
                                    'issue
                                    "#9002"
                                    'notes
                                    "category must match")
                            (hasheq 'file
                                    "tests/resolved.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9003"
                                    'notes
                                    "should be resolved"))))
      (dynamic-wind void
                    (lambda ()
                      (define ledger (load-known-failure-ledger* ledger-path))
                      (check-equal? (length ledger) 3)
                      (define summary
                        (summarize-ledger-results* ledger
                                                   (list (result #:path "tests/known.rkt")
                                                         (result #:path "tests/new.rkt")
                                                         (result #:path "tests/category-changed.rkt"
                                                                 #:out "FAILURE"))))
                      (check-equal? (length (hash-ref summary 'known_failures)) 1)
                      (check-equal? (length (hash-ref summary 'new_failures)) 1)
                      (check-equal? (length (hash-ref summary 'unclassified_failures)) 2)
                      (check-equal? (length (hash-ref summary 'resolved_known_failures)) 1)
                      (check-equal? (hash-ref (car (hash-ref summary 'known_failures)) 'issue)
                                    "#9001"))
                    (lambda () (delete-file/safe ledger-path))))

    (test-case "quarantine expiry boundaries (expires_on is inclusive)"
      ;; W8: while today < expires_on the entry is a tolerated quarantine;
      ;; from expires_on onward (inclusive) it escalates. No expires_on
      ;; (or null) means the entry never expires.
      (check-false (ledger-entry-expired?* (hasheq 'expires_on #f) #:today "2026-08-17"))
      (check-false (ledger-entry-expired?* (hasheq 'expires_on "2026-08-18") #:today "2026-08-17"))
      (check-true (ledger-entry-expired?* (hasheq 'expires_on "2026-08-17") #:today "2026-08-17"))
      (check-true (ledger-entry-expired?* (hasheq 'expires_on "2020-01-01") #:today "2026-08-17"))
      ;; Schema gate: a present expires_on must be ISO YYYY-MM-DD (zero-padded,
      ;; real month/day). Absence (#f) is valid and means "never expires",
      ;; handled by normalize-ledger-entry before the date check.
      (check-false (valid-expires-on?* #f))
      (check-true (valid-expires-on?* "2999-12-31"))
      (check-false (valid-expires-on?* "17/08/2026"))
      (check-false (valid-expires-on?* "2026-13-01"))
      (check-false (valid-expires-on?* "2026-8-17"))
      (check-false (valid-expires-on?* "next tuesday")))

    (test-case "load-known-failure-ledger rejects malformed expires_on"
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    "tests/bad-quarantine.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "1.0.0"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9503"
                                    'notes
                                    "malformed expiry must fail the ledger load"
                                    'expires_on
                                    "17/08/2026"))))
      (dynamic-wind void
                    (lambda ()
                      (check-exn exn:fail? (lambda () (load-known-failure-ledger* ledger-path))))
                    (lambda () (delete-file/safe ledger-path))))

    (test-case "load-known-failure-ledger: missing optional file is an empty ledger"
      ;; W8: the ledger file is optional (no placeholder is committed); a
      ;; missing path must behave as "no known failures", not crash the run.
      (check-equal? (load-known-failure-ledger* (build-path (find-system-path 'temp-dir)
                                                            "q-known-failures-ledger-absent.json"))
                    '()))

    (test-case "expired quarantine entries surface as escalating failures"
      ;; W8: an entry past expires_on is reported as a FAILURE with an
      ;; escalation flag instead of being skipped/tolerated.
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    "tests/quarantine-active.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "1.0.0"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9501"
                                    'notes
                                    "active flake quarantine"
                                    'expires_on
                                    "2999-12-31")
                            (hasheq 'file
                                    "tests/quarantine-expired.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "1.0.0"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9502"
                                    'notes
                                    "stale quarantine"
                                    'expires_on
                                    "2020-01-01"))))
      (dynamic-wind
       void
       (lambda ()
         (define ledger (load-known-failure-ledger* ledger-path))
         (define results
           (list (result #:path "tests/quarantine-active.rkt")
                 (result #:path "tests/quarantine-expired.rkt")))
         (define summary (summarize-ledger-results* ledger results))
         ;; Active quarantine is still tolerated ...
         (check-equal? (length (hash-ref summary 'known_failures)) 1)
         ;; ... but the expired one is an escalating failure.
         (define expired (hash-ref summary 'expired_quarantine_failures '()))
         (check-equal? (length expired) 1)
         (define entry (car expired))
         (check-true (hash-ref entry 'escalate))
         (check-equal? (hash-ref entry 'file) "tests/quarantine-expired.rkt")
         (check-equal? (hash-ref entry 'category) "ASSERTION_FAILURE")
         (check-equal? (hash-ref entry 'expires_on) "2020-01-01")
         (check-equal? (hash-ref entry 'issue) "#9502")
         (define counts (ledger-summary-counts* summary))
         (check-equal? (hash-ref counts 'known_failures) 1)
         (check-equal? (hash-ref counts 'expired_quarantine_failures) 1)
         (check-equal? (hash-ref counts 'new_failures) 0)
         (check-equal? (hash-ref counts 'resolved_known_failures) 0)
         (check-equal? (hash-ref counts 'release_blocking_known_failures) 0)
         ;; Human summary names the escalation explicitly.
         (define out (open-output-string))
         (parameterize ([current-output-port out])
           (print-ledger-summary ledger results))
         (check-true (regexp-match? #rx"Expired quarantine failures:[ ]+1" (get-output-string out))
                     (get-output-string out))
         (check-true (regexp-match? #rx"ESCALATE tests/quarantine-expired" (get-output-string out))
                     (get-output-string out)))
       (lambda () (delete-file/safe ledger-path))))

    (test-case "CLI prints known-failure ledger summary"
      (define missing-file (make-temporary-file "test-missing-cli-known-~a.rkt"))
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    (path->string missing-file)
                                    'category
                                    "MODULE_LOAD_FAILURE"
                                    'owner
                                    "testing"
                                    'first_seen
                                    "0.99.30"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#8313"
                                    'notes
                                    "synthetic missing module"))))
      (call-with-output-file missing-file
                             #:exists 'truncate/replace
                             (lambda (out)
                               (displayln "#lang racket/base" out)
                               (displayln "(require definitely/missing/module)" out)))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture (format "racket scripts/run-tests.rkt --sequential --ledger ~a ~a"
                                ledger-path
                                missing-file)))
         (check-equal? code 1 stderr)
         (check-true (regexp-match? #rx"Known failures:[ ]+1" stdout) stdout)
         (check-true (regexp-match? #rx"New failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"Unclassified failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"Resolved known failures:[ ]+0" stdout) stdout))
       (lambda ()
         (delete-file/safe ledger-path)
         (delete-file/safe missing-file))))

    (test-case "CLI escalates expired quarantine entries as failures"
      ;; W8: past expires_on the entry no longer tolerates the failure —
      ;; the run must FAIL (exit 1) and the summary must carry the
      ;; escalation, so a stale quarantine can never keep a run green.
      (define missing-file (make-temporary-file "test-missing-cli-expired-~a.rkt"))
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    (path->string missing-file)
                                    'category
                                    "MODULE_LOAD_FAILURE"
                                    'owner
                                    "testing"
                                    'first_seen
                                    "0.99.30"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#8314"
                                    'notes
                                    "synthetic expired quarantine"
                                    'expires_on
                                    "2020-01-01"))))
      (call-with-output-file missing-file
                             #:exists 'truncate/replace
                             (lambda (out)
                               (displayln "#lang racket/base" out)
                               (displayln "(require definitely/missing/module)" out)))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture (format "racket scripts/run-tests.rkt --sequential --ledger ~a ~a"
                                ledger-path
                                missing-file)))
         (check-equal? code 1 stderr)
         (check-true (regexp-match? #rx"Known failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"New failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"Expired quarantine failures:[ ]+1" stdout) stdout)
         (check-true (regexp-match? #rx"ESCALATE" stdout) stdout)
         (check-true (regexp-match? #rx"quarantine expired" stdout) stdout))
       (lambda ()
         (delete-file/safe ledger-path)
         (delete-file/safe missing-file))))))
;; W2: standalone runs must fail loudly. A bare (run-tests suite) never sets
;; the process exit code, which previously masked real errors behind exit 0.
(define failures (run-tests suite))

(module+ main
  (when (positive? failures)
    (exit 1)))
