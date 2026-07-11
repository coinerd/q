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

(define (exception-metadata-error layer entry)
  (define name (car entry))
  (define fields (cdr entry))
  (define (field key)
    (and (pair? fields) (assoc key fields)))
  (define (value key)
    (define found (field key))
    (and found (cdr found)))
  (define revisit-field (field 'revisit-by))
  (define permanent-field (field 'permanent-waiver))
  (define justification-field (field 'waiver-justification))
  (cond
    [(not (and (string? (value 'owner)) (non-empty-string? (value 'owner))))
     (format "~a/~a: missing owner" layer name)]
    [(not (and (string? (value 'rationale)) (non-empty-string? (value 'rationale))))
     (format "~a/~a: missing rationale" layer name)]
    [(and revisit-field permanent-field) (format "~a/~a: has both lifecycle fields" layer name)]
    [revisit-field
     (cond
       [justification-field (format "~a/~a: dated exception has waiver justification" layer name)]
       [(not (and (string? (cdr revisit-field))
                  (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" (cdr revisit-field))))
        (format "~a/~a: malformed revisit-by" layer name)]
       [else #f])]
    [permanent-field
     (cond
       [(not (eq? (cdr permanent-field) #t)) (format "~a/~a: permanent-waiver must be #t" layer name)]
       [(not (and justification-field
                  (string? (cdr justification-field))
                  (non-empty-string? (cdr justification-field))))
        (format "~a/~a: permanent waiver lacks justification" layer name)]
       [else #f])]
    [else (format "~a/~a: missing lifecycle metadata" layer name)]))

(define boundary-tests
  (test-suite "architecture-boundaries"

    (test-case "Only known exceptions in runtime/ import from tools/ or extensions/"
      ;; Use recursive scan to include subdirectory modules (runtime/iteration/*, etc.)
      (define runtime-files (rkt-files-in-recursive "runtime"))
      (define runtime-exc (policy-ref 'known-exceptions 'runtime))
      ;; Policy names are canonical paths relative to runtime/. A top-level name
      ;; therefore cannot accidentally waive a nested module with the same basename.
      (define known-exceptions
        (for/set ([entry (in-list runtime-exc)])
          (define name (car entry))
          (if (symbol? name)
              (symbol->string name)
              name)))
      (define (known-exception? f)
        (define rel (path->string (find-relative-path (build-path q-dir "runtime") f)))
        (set-member? known-exceptions rel))
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

    (test-case "All boundary exceptions have valid lifecycle metadata"
      (define layers-with-exceptions (map car (policy-ref 'known-exceptions)))
      (define metadata-errors
        (for*/list ([layer (in-list layers-with-exceptions)]
                    [entry (in-list (policy-ref 'known-exceptions layer))])
          (exception-metadata-error layer entry)))
      (check-equal? (filter identity metadata-errors)
                    '()
                    (format "Invalid boundary exception metadata: ~a"
                            (filter identity metadata-errors)))
      (define valid-base '((rationale . "reason") (owner . "owner")))
      (define fixtures
        (list (cons (append valid-base '((revisit-by . "2027-01-01"))) #f)
              (cons (append valid-base
                            '((permanent-waiver . #t) (waiver-justification . "intentional")))
                    #f)
              (cons valid-base #t)
              (cons (append valid-base
                            '((revisit-by . "2027-01-01") (permanent-waiver . #t)
                                                          (waiver-justification . "ambiguous")))
                    #t)
              (cons (append valid-base '((permanent-waiver . #f))) #t)
              (cons (append valid-base
                            '((permanent-waiver . permanent) (waiver-justification . "wrong type")))
                    #t)
              (cons (append valid-base '((permanent-waiver . #t) (waiver-justification . ""))) #t)
              (cons (append valid-base '((revisit-by . "January 1"))) #t)
              (cons (append valid-base '((revisit-by . 20270101))) #t)
              (cons (append valid-base
                            '((revisit-by . "2027-01-01") (waiver-justification . "not a waiver")))
                    #t)))
      (for ([fixture (in-list fixtures)])
        (define error (exception-metadata-error 'fixture (cons 'sample.rkt (car fixture))))
        (check-equal? (and error #t) (cdr fixture)))
      (define layer-adapter-fields
        (cdr (assoc 'layer-adapters.rkt (policy-ref 'known-exceptions 'runtime))))
      (check-equal? (cdr (assoc 'permanent-waiver layer-adapter-fields)) #t)
      (check-false (assoc 'revisit-by layer-adapter-fields)))

    (test-case "No boundary exception has expired revisit-by date"
      ;; W25 blocking gate: expired revisit-by dates must be addressed.
      ;; Uses simple YYYY-MM-DD string comparison (ISO format sorts lexicographically).
      (define layers-with-exceptions (map car (policy-ref 'known-exceptions)))
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
