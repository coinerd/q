#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-wave-doc-lint.rkt
;; BUG-0041 characterization pin (v1.00.21 W0; FLIPPED by W4).
;;
;; TODAY wave docs are never linted at /go entry: a W0 doc that omits
;; ## Files, ## Verify, and ## Done loads cleanly through the whole
;; plan-validation path — zero warnings, zero errors, no diagnostic
;; channel even exists. Every assertion below PASSES against today's
;; red behavior; W4 flips them once /go lints wave-doc sections.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/runtime-path
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/campaign-repository.rkt")

(define-runtime-path wave-docs-src "../extensions/gsd/wave-docs.rkt")
(define-runtime-path plan-validator-src "../extensions/gsd/plan-validator.rkt")

;; ── Fixture: a minimal .planning whose W0 doc has no sections ──

(define (make-temp-planning)
  (define dir (make-temporary-file "w0-lint~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Lint characterization")
                         (newline)
                         (displayln "- [Inbox] W0: Doc with no sections → waves/W0-no-sections.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-no-sections.md")
                       (lambda ()
                         (displayln "# Wave 0: Doc with no sections")
                         (displayln "Status: Inbox")
                         (newline)
                         (displayln "## Goal")
                         (newline)
                         (displayln "Missing ## Files, ## Verify, and ## Done entirely."))
                       #:exists 'replace)
  dir)

(define (cleanup! dir)
  (delete-directory/files dir #:must-exist? #f))

(define w0-lint-suite
  (test-suite "BUG-0041: wave-doc lint is absent — section-less docs load silently"

    (test-case "section-less wave doc parses silently — nothing flags it"
      (define dir (make-temp-planning))
      (dynamic-wind void
                    (lambda ()
                      (define idx
                        (parse-plan-index (file->string (build-path dir ".planning" "PLAN.md"))))
                      (check-equal? (and (list? idx) (= (length idx) 1))
                                    #t
                                    "index entry parses despite the doc missing every section")
                      ;; non-vacuous precondition: the fixture is visible to the tooling
                      (check-equal? (wave-exists? dir 0 "no-sections") #t)
                      (define doc (read-wave-doc dir 0 "no-sections"))
                      (check-equal? (hash? doc) #t "doc loads without error")
                      ;; absent seam: the parser extracts ONLY the status header —
                      ;; there is no Files/Verify/Done key, extraction, or storage
                      (check-equal? (sort (hash-keys doc) symbol<?)
                                    '(content index path slug status)
                                    "no section data is parsed or represented at all"))
                    (lambda () (cleanup! dir))))

    (test-case "status consistency reports nothing for the section-less doc"
      (define dir (make-temp-planning))
      (dynamic-wind void
                    (lambda ()
                      ;; non-vacuous precondition: PLAN.md is where the checker looks
                      (check-equal? (file-exists? (build-path dir ".planning" "PLAN.md")) #t)
                      (check-equal? (check-status-consistency dir) '()))
                    (lambda () (cleanup! dir))))

    (test-case "the only existing warning channel (plan-format deprecations) is silent"
      (define dir (make-temp-planning))
      (dynamic-wind void
                    (lambda () (check-equal? (plan-format-deprecation-warning-lines dir) '()))
                    (lambda () (cleanup! dir))))

    (test-case "the /go load path ingests the section-less plan with zero diagnostics"
      (define dir (make-temp-planning))
      (dynamic-wind void
                    (lambda ()
                      ;; contract: 1 result value; a truthy campaign-record comes back
                      (define vals
                        (call-with-values (lambda () (load-or-migrate-campaign! dir)) list))
                      (check-equal? (length vals) 1 "single result value")
                      (define rec (car vals))
                      (check-equal? (and (struct? rec) #t)
                                    #t
                                    "migrate + load succeeds; no warning/error can surface"))
                    (lambda () (cleanup! dir))))

    ;; ── Absent-seam marker (v1.00.19 freshness-pin precedent) ────

    (test-case "no wave-doc lint exists anywhere in the plan tooling"
      (check-equal? (file-exists? wave-docs-src) #t)
      (check-false (string-contains? (file->string wave-docs-src) "lint")
                   "TODAY wave-docs.rkt has no lint function — the seam is absent")
      (when (file-exists? plan-validator-src)
        (check-false (string-contains? (file->string plan-validator-src) "lint")
                     "TODAY plan-validator.rkt does not lint wave docs")))))

(module+ test
  (run-tests w0-lint-suite))

(module+ main
  (unless (zero? (run-tests w0-lint-suite))
    (exit 1)))
