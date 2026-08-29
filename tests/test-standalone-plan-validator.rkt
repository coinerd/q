#lang racket

;;; test-standalone-plan-validator.rkt — W4 flip of the W0
;;; characterization pin for BUG-0048. The standalone plan-validation
;;; CLI now EXISTS (scripts/validate-plan.rkt): it validates a good
;;; plan cleanly (exit 0), names violations on a bad plan (exit 1),
;;; and reports the mechanical file-attribution sanity check
;;; (declared `- File:` paths that do not exist under the base).

(require rackunit
         racket/file
         racket/path
         racket/system
         racket/port)

(define repo-root
  (simplify-path (build-path (find-system-path 'run-file) 'up 'up)))

(define scripts-dir (build-path repo-root "scripts"))
(define cli (build-path scripts-dir "validate-plan.rkt"))
(define racket-bin (find-executable-path "racket"))

;; --- Pin 1 (flipped): the standalone validator CLI EXISTS.
(check-true
 (file-exists? cli)
 "scripts/validate-plan.rkt exists (BUG-0048 closed)")

;; --- Helper: run the CLI in an isolated fixture project.
;; run-cli : path? -> (values exit-code stdout-string)
(define (run-cli fixture-dir)
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory fixture-dir]
                   [current-output-port out]
                   [current-error-port out])
      (system/exit-code (format "~a ~a" racket-bin cli))))
  (values code (get-output-string out)))

(define (make-fixture)
  (make-temporary-file "validate-plan-test-~a" #f
                       (find-system-path 'temp-dir)))

(define (make-fixture-dir)
  (define f (make-fixture))
  (when (file-exists? f) (delete-file f))
  (make-directory f)
  (simplify-path f))

;; Good wave doc: `# Wave N` header, canonical `Status:` line,
;; non-empty Files/Verify/Done sections. No stray body `Status:` line.
(define (good-wave-doc idx title files)
  (format "# Wave ~a: ~a\n\nStatus: Inbox\n\n## Root Cause\n\ncause ~a\n\n## Files\n\n~a\n## Verify\n\nracket tests/x.rkt\n\n## Done\n\nDone means closed.\n"
          idx title idx
          (string-join (for/list ([f files]) (format "- File: ~a" f)) "\n")))

;; --- Pin 2: a GOOD plan validates cleanly (exit 0, OK line).
(define good-dir (make-fixture-dir))
(make-directory* (build-path good-dir ".planning" "waves"))
(display-to-file
 "- [Inbox] W0: Fixture Wave → waves/W0-fixture-wave.md\n"
 (build-path good-dir ".planning" "PLAN.md")
 #:exists 'replace)
;; File targets point at files that exist under the fixture base.
(display-to-file "" (build-path good-dir "existing-file.rkt") #:exists 'replace)
(display-to-file
 (good-wave-doc 0 "Fixture Wave" (list "existing-file.rkt"))
 (build-path good-dir ".planning" "waves" "W0-fixture-wave.md")
 #:exists 'replace)

(define-values (good-code good-out) (run-cli good-dir))
(check-equal? good-code 0 "good fixture plan exits 0")
(check-not-false (regexp-match? #rx"OK" good-out) "good plan reports OK")

;; --- Pin 3: a BAD plan (index entry with no wave doc on disk) is
;; rejected with exit 1 and a NAMED violation — the same strict
;; missing-doc check /go runs.
(define bad-dir (make-fixture-dir))
(make-directory* (build-path bad-dir ".planning" "waves"))
(display-to-file
 "- [Inbox] W0: Missing Doc → waves/W0-missing-doc.md\n"
 (build-path bad-dir ".planning" "PLAN.md")
 #:exists 'replace)

(define-values (bad-code bad-out) (run-cli bad-dir))
(check-equal? bad-code 1 "plan with missing wave doc exits 1")
(check-not-false (regexp-match? #rx"FAILED" bad-out) "bad plan reports FAILED")
(check-not-false (regexp-match? #rx"ERROR" bad-out) "bad plan names an ERROR")
(check-not-false
 (regexp-match? #rx"W0-missing-doc" bad-out)
 "the named missing wave doc appears in the report")

;; --- Pin 4: file-attribution sanity check — a declared `- File:`
;; path that does not exist under the base is named as a warning
;; (attribution), and never blocks a structurally good plan.
(define attr-dir (make-fixture-dir))
(make-directory* (build-path attr-dir ".planning" "waves"))
(display-to-file
 "- [Inbox] W0: Attr Wave → waves/W0-attr-wave.md\n"
 (build-path attr-dir ".planning" "PLAN.md")
 #:exists 'replace)
(display-to-file
 (good-wave-doc 0 "Attr Wave" (list "q/nowhere/misattributed.rkt"))
 (build-path attr-dir ".planning" "waves" "W0-attr-wave.md")
 #:exists 'replace)

(define-values (attr-code attr-out) (run-cli attr-dir))
(check-equal? attr-code 0 "attribution warning does not block (exit 0)")
(check-not-false
 (regexp-match? #rx"q/nowhere/misattributed\\.rkt" attr-out)
 "misattributed path is named in the report")
(check-not-false
 (regexp-match? #rx"ATTRIBUTION-WARN" attr-out)
 "attribution finding is labelled as a warning, not an error")

;; --- Pin 5: shared kernel — the CLI and /go cannot diverge. The
;; kernel validate-plan-artifacts lives in wave-executor.rkt and is
;; the SAME entry /go's validate-plan-for-go delegates to.
(check-not-false
 (regexp-match? #rx"validate-plan-artifacts"
                (file->string (build-path repo-root "extensions" "gsd" "command-handlers.rkt")))
 "/go delegates its plan validation to the shared kernel")

;; Cleanup fixtures.
(for ([d (in-list (list good-dir bad-dir attr-dir))])
  (delete-directory/files d))

(displayln "PASS test-standalone-plan-validator (BUG-0048: standalone plan validator exists and works)")
