#lang racket

;; @covers extensions/gsd/planning-artifact-validation.rkt
;; @covers extensions/gsd/wave-executor.rkt
;; @speed fast  ;; @suite extensions
;; @boundary unit

;; tests/test-gsd-plan-validation.rkt — W1 (GSD-SIMPLIFICATION-PLAN)
;; "One planning, path, Verify, and preflight contract."
;;
;; Pins the consolidation this wave performed:
;;   1. ONE wave-document path grammar (W<n>/WAVE-<n> prefixes, safe dotted
;;      slugs, unchanged traversal/absolute/NUL refusals), consumed by
;;      planning-artifact normalization and by preflight;
;;   2. the shared validate-plan-artifacts kernel re-homed into
;;      planning-artifact-validation.rkt and still re-provided by
;;      wave-executor.rkt (same constructors/accessors, same error text,
;;      same exit-code contract);
;;   3. structured findings: stable machine code + named recovery action +
;;      artifact references on every finding;
;;   4. all independent issues are reported: empty index, missing index row,
;;      missing document, mapping ambiguity, invalid Verify span, declared
;;      path bindability, snapshot drift class.
;;
;; The same checks are also reachable through the historical
;; tests/test-gsd-planning-preflight-kernel.rkt alias.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         "../extensions/gsd/planning-artifact-validation.rkt"
         (prefix-in legacy: "../extensions/gsd/wave-executor.rkt"))

;; ------------------------------------------------------------
;; Fixtures
;; ------------------------------------------------------------

(define (make-fixture)
  (define dir (make-temporary-file "gsd-plan-validation-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (simplify-path dir))

(define (drop-fixture dir)
  (delete-directory/files dir #:must-exist? #f))

(define (put-plan! dir rows)
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Fixture\n\n## Waves\n\n" out)
                           (for ([row (in-list rows)])
                             (display row out)
                             (newline out)))
                         #:exists 'truncate))

;; A wave doc satisfying every executor-contract section, with an accepted
;; Verify command span.
(define (put-wave-doc! dir idx slug #:verify [verify "raco test tests/x.rkt"])
  (define path (build-path dir ".planning" "waves" (format "W~a-~a.md" idx slug)))
  (call-with-output-file path
                         (lambda (out)
                           (fprintf out "# Wave ~a\nStatus: Inbox\n\n" idx)
                           (display "## Files\n\n" out)
                           (fprintf out "- File: existing-file.rkt\n\n## Verify\n\n~a\n\n" verify)
                           (display "## Done\n\nDone means closed.\n" out))
                         #:exists 'truncate)
  (unless (file-exists? (build-path dir "existing-file.rkt"))
    (display-to-file "" (build-path dir "existing-file.rkt") #:exists 'replace))
  path)

(define (codes result)
  (hash-ref result 'codes))

(define (finding-with-code result code)
  (for/first ([f (in-list (hash-ref result 'findings))]
              #:when (eq? (plan-validation-finding-code f) code))
    f))

;; ------------------------------------------------------------
;; 1. One wave-document path grammar
;; ------------------------------------------------------------

(test-case "wave path grammar: canonical W<n> form with a dotted slug is valid"
  (check-true (wave-doc-rel-path? "waves/W0-racket-9.3.md"))
  (check-equal? (wave-doc-rel-path-index "waves/W0-racket-9.3.md") 0)
  (check-false (wave-doc-rel-path-refusal "waves/W0-racket-9.3.md")))

(test-case "wave path grammar: WAVE-<n> long form is valid and resolves its index"
  (check-equal? (wave-doc-rel-path-index "waves/WAVE-12-add-tests.md") 12)
  (check-true (wave-doc-rel-path? "waves/WAVE-0-fix_bug.md")))

(test-case "wave path grammar: unsafe shapes stay refused"
  (for ([bad (in-list (list ""
                            "/abs/waves/W0-x.md"
                            "waves/../etc.md"
                            "waves/W0-.md"
                            "waves/W0-a/b.md"
                            "waves/W0-a.txt"
                            "waves/no-md"
                            "waves/W0-a\x00b.md"
                            "PLAN.md"))])
    (check-false (wave-doc-rel-path? bad) bad))
  (check-true (string? (wave-doc-rel-path-refusal "waves/../etc.md"))))

(test-case "planning-artifact names share the same grammar"
  ;; Wave-prefixed names must satisfy the canonical grammar.
  (check-false (wave-artifact-name-refusal "waves/W0-racket-9.3.md"))
  (check-false (wave-artifact-name-refusal "waves/WAVE-3-long-form.md"))
  (check-true (string? (wave-artifact-name-refusal "waves/W0-.md")))
  (check-true (string? (wave-artifact-name-refusal "waves/../etc.md")))
  (check-true (string? (wave-artifact-name-refusal "waves/W0-a\x00b.md")))
  ;; Non-wave documents below waves/ keep their historical acceptance.
  (check-false (wave-artifact-name-refusal "waves/notes.md"))
  ;; Non-waves/ names are the command-normalization owner's business: this
  ;; grammar reports them as out of its scope, never as valid.
  (check-true (string? (wave-artifact-name-refusal "PLAN"))))

;; ------------------------------------------------------------
;; 2. Structured finding contract (legacy constructor preserved)
;; ------------------------------------------------------------

(test-case "finding: legacy 3-argument constructor still works"
  (define f (plan-validation-finding #f 'error "boom"))
  (check-true (plan-validation-finding? f))
  (check-equal? (plan-validation-finding-wave-idx f) #f)
  (check-equal? (plan-validation-finding-kind f) 'error)
  (check-equal? (plan-validation-finding-message f) "boom")
  (check-equal? (plan-validation-finding-code f) 'plan-error)
  (check-false (plan-validation-finding-next-action f))
  (check-equal? (plan-validation-finding-artifacts f) '()))

(test-case "finding: structured fields are carried"
  (define f
    (plan-validation-finding 2
                             'warning
                             "named"
                             #:code 'wave-doc-slug-mismatch
                             #:next-action "fix the arrow"
                             #:artifacts (list ".planning/waves/W2-x.md")))
  (check-equal? (plan-validation-finding-wave-idx f) 2)
  (check-equal? (plan-validation-finding-code f) 'wave-doc-slug-mismatch)
  (check-equal? (plan-validation-finding-next-action f) "fix the arrow")
  (check-equal? (plan-validation-finding-artifacts f) (list ".planning/waves/W2-x.md")))

;; ------------------------------------------------------------
;; 3. Kernel: hard refusals keep their legacy text and gain codes
;; ------------------------------------------------------------

(test-case "kernel: no PLAN keeps the legacy message and gains plan-missing"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (define result (validate-plan-artifacts dir))
                  (check-false (hash-ref result 'ok?))
                  (check-equal? (hash-ref result 'error-message)
                                "No PLAN found in .planning/. Use /plan <task> to create one.")
                  (check-equal? (codes result) '(plan-missing))
                  (check-equal? (hash-ref (hash-ref result 'diagnostics) 'code) 'plan-missing)
                  (check-true (string? (hash-ref (hash-ref result 'diagnostics) 'next-action))))
                (lambda () (drop-fixture dir))))

(test-case "kernel: empty index keeps the legacy no-waves diagnostic"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '())
                  (define result (validate-plan-artifacts dir))
                  (check-false (hash-ref result 'ok?))
                  (check-true (string-prefix? (hash-ref result 'error-message) "Plan has no waves\n"))
                  (check-equal? (codes result) '(plan-no-waves)))
                (lambda () (drop-fixture dir))))

(test-case "kernel: inline-only plan keeps the BUG-0023 residual rejection"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (display-to-file "# Plan\n\n## Wave 0: inline\n"
                                   (build-path dir ".planning" "PLAN.md")
                                   #:exists 'replace)
                  (define result (validate-plan-artifacts dir))
                  (check-false (hash-ref result 'ok?))
                  (check-true (string-contains? (hash-ref result 'error-message)
                                                "no longer accepted at /go"))
                  (check-equal? (codes result) '(plan-inline-format-deprecated)))
                (lambda () (drop-fixture dir))))

(test-case "kernel: a good plan is /go-ready with no error findings"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '("- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md"))
                  (put-wave-doc! dir 0 "fixture-wave")
                  (define result (validate-plan-artifacts dir))
                  (check-true (hash-ref result 'ok?) (hash-ref result 'error-message))
                  (check-equal? (codes result) '())
                  (check-equal? (hash-ref (hash-ref result 'diagnostics) 'code) 'plan-ready)
                  (check-not-false (hash-ref result 'plan)))
                (lambda () (drop-fixture dir))))

(test-case "kernel: every missing document is an independent named finding"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir
                             (list "- [Inbox] W0: One → waves/W0-one.md"
                                   "- [Inbox] W1: Two → waves/W1-two.md"))
                  (define result (validate-plan-artifacts dir))
                  (check-false (hash-ref result 'ok?))
                  (check-equal? (codes result) '(wave-doc-missing wave-doc-missing))
                  ;; the aggregated legacy text still names both documents
                  (check-true (string-contains? (hash-ref result 'error-message) "W0-one.md"))
                  (check-true (string-contains? (hash-ref result 'error-message) "W1-two.md"))
                  ;; artifacts carry both machine-checkable paths
                  (check-equal? (sort (hash-ref result 'artifacts) string<?)
                                (list ".planning/waves/W0-one.md" ".planning/waves/W1-two.md")))
                (lambda () (drop-fixture dir))))

(test-case "kernel: ambiguous shared carrier still blocks and names the document"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir
                             (list "- [Inbox] W0: carrier → waves/W0-carrier.md"
                                   "- [Inbox] W2: twin → waves/W0-carrier.md"))
                  (put-wave-doc! dir 0 "carrier")
                  (define result (validate-plan-artifacts dir))
                  (check-false (hash-ref result 'ok?))
                  (check-not-false (memq 'wave-doc-mapping-ambiguous (codes result)))
                  (check-true (string-contains? (hash-ref result 'error-message)
                                                ".planning/waves/W0-carrier.md")))
                (lambda () (drop-fixture dir))))

;; ------------------------------------------------------------
;; 4. Independent advisory issues are reported without blocking
;; ------------------------------------------------------------

(test-case "kernel: a wave document with no index row is reported (missing row)"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '("- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md"))
                  (put-wave-doc! dir 0 "fixture-wave")
                  (put-wave-doc! dir 3 "orphan-wave")
                  (define result (validate-plan-artifacts dir))
                  (check-true (hash-ref result 'ok?) (hash-ref result 'error-message))
                  (define orphan (finding-with-code result 'wave-doc-without-index-row))
                  (check-not-false orphan)
                  (check-equal? (plan-validation-finding-kind orphan) 'warning)
                  (check-true (string-contains? (plan-validation-finding-message orphan)
                                                "W3-orphan-wave.md")))
                (lambda () (drop-fixture dir))))

(test-case "kernel: an invalid Verify span is named with its reason, not blocked"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '("- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md"))
                  (put-wave-doc! dir 0 "fixture-wave" #:verify "- `find . -name x.rkt` exits 1")
                  (define result (validate-plan-artifacts dir))
                  (check-true (hash-ref result 'ok?) (hash-ref result 'error-message))
                  (define verify-finding (finding-with-code result 'verify-declaration-rejected))
                  (check-not-false verify-finding)
                  (check-true (string-contains? (plan-validation-finding-message verify-finding)
                                                "find . -name x.rkt"))
                  (check-true (string-contains? (plan-validation-finding-message verify-finding)
                                                "unsupported-command")))
                (lambda () (drop-fixture dir))))

(test-case "kernel: a dotted declared wave path loads but names its bindability gap"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '("- [Inbox] W0: Racket upgrade → waves/W0-racket-9.3.md"))
                  (put-wave-doc! dir 0 "racket-9.3")
                  (define result (validate-plan-artifacts dir))
                  ;; The grammar accepts it (no path-invalid error) and the document loads.
                  (check-true (hash-ref result 'ok?) (hash-ref result 'error-message))
                  (check-equal? (codes result) '())
                  (define bind (finding-with-code result 'wave-doc-path-not-bindable))
                  (check-not-false bind "the frozen-snapshot reference grammar gap is named")
                  (check-true (string-contains? (plan-validation-finding-message bind)
                                                ".planning/waves/W0-racket-9.3.md")))
                (lambda () (drop-fixture dir))))

;; ------------------------------------------------------------
;; 5. Loader contract preserved (single implementation, legacy re-export)
;; ------------------------------------------------------------

(test-case "loader: #f without PLAN.md, #f on an empty index"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (check-false (load-plan-from-index dir))
                  (put-plan! dir '())
                  (check-false (load-plan-from-index dir))
                  (check-false (legacy:load-plan-from-index dir)))
                (lambda () (drop-fixture dir))))

(test-case "loader: a missing declared document is refused by name"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (put-plan! dir '("- [Inbox] W0: Missing → waves/W0-missing.md"))
                  (check-exn (lambda (e)
                               (and (exn:fail? e)
                                    (string-contains? (exn-message e)
                                                      ".planning/waves/W0-missing.md")))
                             (lambda () (load-plan-from-index dir)))
                  ;; the legacy import path is the same procedure
                  (check-exn exn:fail? (lambda () (legacy:load-plan-from-index dir))))
                (lambda () (drop-fixture dir))))

(test-case "kernel: wave-executor's re-export is the same implementation"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (put-plan! dir
                (list "- [Inbox] W0: One → waves/W0-one.md" "- [Inbox] W1: Two → waves/W1-two.md"))
     (define via-kernel (validate-plan-artifacts dir))
     (define via-legacy (legacy:validate-plan-artifacts dir))
     (check-equal? (codes via-legacy) (codes via-kernel))
     (check-equal? (hash-ref via-legacy 'error-message) (hash-ref via-kernel 'error-message))
     ;; the legacy accessors/constructor surface is still provided
     (check-true (procedure? legacy:plan-validation-finding))
     (check-equal?
      (legacy:plan-validation-finding-code (legacy:plan-validation-finding #f 'error "boom"))
      'plan-error))
   (lambda () (drop-fixture dir))))

(displayln "PASS test-gsd-plan-validation (W1: one planning/path/Verify/preflight kernel)")
