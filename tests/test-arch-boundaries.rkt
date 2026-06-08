#lang racket

;; @speed fast  ;; @suite arch

;; BOUNDARY: integration

;; tests/test-arch-boundaries.rkt — Architecture layer boundary tests
;;
;; Verifies that layering constraints are maintained:
;;   - Only known exceptions in runtime/ may import from tools/ or extensions/
;;   - TUI modules must not import from llm/, tools/
;;   - extensions/ must not import from tui/ (with known exceptions)
;;   - llm/ must not import from runtime/, tools/, extensions/
;;
;; Data source: docs/architecture/dependency-policy.rktd
;; Refs: #432, ARCH-01

(require rackunit
         rackunit/text-ui
         "helpers/arch-utils.rkt"
         racket/date)

;; Load policy from single source of truth
(define policy-path (build-path q-dir "docs" "architecture" "dependency-policy.rktd"))

(define policy (call-with-input-file policy-path read))

(define (policy-ref section . keys)
  (let loop ([data (cdr (assoc section policy))]
             [ks keys])
    (if (null? ks)
        data
        (loop (cdr (assoc (car ks) data)) (cdr ks)))))

;; ============================================================
;; Boundary tests
;; ============================================================

(define boundary-tests
  (test-suite "architecture-boundaries"

    (test-case "Only known exceptions in runtime/ import from tools/ or extensions/"
      ;; Use recursive scan to include subdirectory modules (runtime/iteration/*, etc.)
      (define runtime-files (rkt-files-in-recursive "runtime"))
      (define runtime-exc (policy-ref 'known-exceptions 'runtime))
      ;; Build set of known exception RELATIVE paths (e.g. "runtime/iteration/loop-config.rkt")
      ;; Each entry is either (filename . "rationale") or (filename (rationale . "...") ...)
      (define known-exceptions
        (for/fold ([acc (set)]) ([entry (in-list runtime-exc)])
          (define name
            (if (symbol? (car entry))
                (symbol->string (car entry))
                (car entry)))
          ;; Accept both bare filename and runtime/ prefixed path
          (set-add (set-add acc name) (string-append "runtime/" name))))
      ;; Also match filenames for backward compat
      (define (known-exception? f)
        (define rel (path->string (find-relative-path (build-path q-dir "runtime") f)))
        (define bare (path->string (file-name-from-path f)))
        (or (set-member? known-exceptions rel)
            (set-member? known-exceptions bare)
            (set-member? known-exceptions (string-append "runtime/" rel))
            (set-member? known-exceptions (string-append "runtime/" bare))))
      (define violations
        (for/list ([f (in-list runtime-files)]
                   #:when (not (known-exception? f)))
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))
              (format "~a: upward imports detected" (find-relative-path q-dir f))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "Unexpected upward imports in runtime/: ~a" actual-violations)))

    (test-case "TUI modules must not import from llm/, tools/"
      (define tui-files (rkt-files-in "tui"))
      (define violations
        (for/list ([f (in-list tui-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../llm/" "../../llm/" "../tools/" "../../tools/"))
              (format "~a: imports from forbidden layer" (file-name-from-path f))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI modules importing from forbidden layers: ~a" actual-violations)))

    (test-case "All boundary exceptions have required metadata (owner, revisit-by)"
      ;; W25 blocking gate: every known exception must have owner and revisit-by fields.
      ;; Undocumented exceptions (missing metadata) cause test failure.
      (define layers-with-exceptions '(runtime extensions))
      (define missing-metadata
        (for*/list ([layer (in-list layers-with-exceptions)]
                    [entry (in-list (policy-ref 'known-exceptions layer))])
          (define name (car entry))
          (define fields (cdr entry))
          (define (has-field? f)
            (and (pair? fields) (assoc f fields)))
          (cond
            [(not (has-field? 'owner)) (format "~a/~a: missing owner" layer name)]
            [(not (has-field? 'revisit-by)) (format "~a/~a: missing revisit-by" layer name)]
            [else #f])))
      (define actual-missing (filter identity missing-metadata))
      (check-equal? actual-missing
                    '()
                    (format "Boundary exceptions missing metadata: ~a" actual-missing)))

    (test-case "No boundary exception has expired revisit-by date"
      ;; W25 blocking gate: expired revisit-by dates must be addressed.
      ;; Uses simple YYYY-MM-DD string comparison (ISO format sorts lexicographically).
      (define layers-with-exceptions '(runtime extensions))
      (define today-str
        (parameterize ([date-display-format 'iso-8601])
          (date->string (seconds->date (current-seconds)))))
      (define expired
        (for*/list ([layer (in-list layers-with-exceptions)]
                    [entry (in-list (policy-ref 'known-exceptions layer))])
          (define name (car entry))
          (define fields (cdr entry))
          (define revisit (and (pair? fields) (assoc 'revisit-by fields)))
          (if revisit
              (let ([date-str (cdr revisit)])
                (if (string<? date-str today-str)
                    (format "~a/~a: revisit-by ~a has expired" layer name date-str)
                    #f))
              #f)))
      (define actual-expired (filter identity expired))
      (check-equal? actual-expired
                    '()
                    (format "Expired boundary exceptions need reassessment: ~a" actual-expired)))

    (test-case "Recursive boundary scan includes subdirectory modules"
      ;; Prove that rkt-files-in-recursive catches files in nested directories
      ;; that rkt-files-in would miss. This is the core fix from W0.
      (define flat-files (rkt-files-in "runtime"))
      (define recursive-files (rkt-files-in-recursive "runtime"))
      ;; Recursive must be a strict superset
      (check-true (> (length recursive-files) (length flat-files))
                  (format "Recursive scan (~a files) must find more than flat scan (~a files)"
                          (length recursive-files)
                          (length flat-files)))
      ;; Specifically, runtime/iteration/ subdirectory files must be included
      (define iteration-files
        (filter (λ (f) (string-contains? (path->string f) "iteration/")) recursive-files))
      (check-true (>= (length iteration-files) 5)
                  (format "runtime/iteration/ should have 5+ files, found ~a"
                          (length iteration-files)))
      ;; runtime/iteration/decision.rkt must be in the recursive list
      (define decision-file
        (findf (λ (f) (string-suffix? (path->string f) "decision.rkt")) recursive-files))
      (check-not-false decision-file
                       "runtime/iteration/decision.rkt must be found by recursive scan"))))

(run-tests boundary-tests)
