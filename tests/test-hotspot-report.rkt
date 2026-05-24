#lang racket

;; tests/test-hotspot-report.rkt — Tests for hotspot scoring logic

(require rackunit
         rackunit/text-ui
         json
         racket/port)

;; ── Helper ──

;; Resolve q/ directory: raco test changes CWD, so we need robust path resolution.
;; Use the source file's directory (tests/) and go up one level to q/.
(define q-dir
  (simplify-path
   (build-path (or (current-load-relative-directory)
                   (let ([src (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference)))])
                     (if (path? src)
                         (path-only src)
                         (current-directory))))
               'up)))

(define (run-hotspot-report . args)
  (define racket-path (or (find-executable-path "racket") (error "racket not found")))
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (apply subprocess #f #f #f racket-path "scripts/hotspot-report.rkt" args)))
  (close-output-port in)
  (define stdout-text (port->string out))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait sp)
  (define code (subprocess-status sp))
  (values code stdout-text))

;; ── Tests ──

(define hotspot-tests
  (test-suite "hotspot-report"

    (test-case "default report shows top hotspots"
      (define-values (code out) (run-hotspot-report))
      (check-equal? code 0 "should exit 0")
      (check-true (string-contains? out "Score = Change Frequency") "should show scoring formula")
      (check-true (string-contains? out "Total source files:") "should show total file count"))

    (test-case "JSON output is valid and has required fields"
      (define-values (code out) (run-hotspot-report "--json"))
      (check-equal? code 0)
      (define data (string->jsexpr out))
      (check-true (hash-has-key? data 'hotspots))
      (define hotspots (hash-ref data 'hotspots))
      (check-true (> (length hotspots) 0))
      ;; Each hotspot must have file, loc, frequency, score
      (for ([h (in-list hotspots)])
        (check-true (hash-has-key? h 'file))
        (check-true (hash-has-key? h 'loc))
        (check-true (hash-has-key? h 'frequency))
        (check-true (hash-has-key? h 'score))))

    (test-case "score equals frequency times loc"
      (define-values (code out) (run-hotspot-report "--json" "--all"))
      (define data (string->jsexpr out))
      (define hotspots (hash-ref data 'hotspots))
      ;; Verify score = freq × loc for every entry with non-zero values
      (for ([h (in-list hotspots)]
            #:when (and (> (hash-ref h 'frequency) 0) (> (hash-ref h 'loc) 0)))
        (check-equal? (hash-ref h 'score)
                      (* (hash-ref h 'frequency) (hash-ref h 'loc))
                      (format "score mismatch for ~a" (hash-ref h 'file)))))

    (test-case "hotspots are sorted descending by score"
      (define-values (code out) (run-hotspot-report "--json" "--all"))
      (define data (string->jsexpr out))
      (define scores (map (λ (h) (hash-ref h 'score)) (hash-ref data 'hotspots)))
      (check-true (apply >= scores) "scores should be in descending order"))

    (test-case "CI mode exits 0 (non-blocking)"
      (define-values (code out) (run-hotspot-report "--ci"))
      (check-equal? code 0 "CI mode should not fail (non-blocking)"))

    (test-case "Hotspot budget: files exceeding block-threshold have risk notes"
      ;; W26 blocking gate: files with score > 20000 must have a risk-note entry
      ;; in the dependency policy's hotspot-budget section.
      (define policy-path (build-path q-dir "docs" "architecture" "dependency-policy.rktd"))
      (define policy (call-with-input-file policy-path read))
      (define hotspot-section (let ([hb (assoc 'hotspot-budget policy)]) (and hb (cdr hb))))
      (define block-threshold (cdr (assoc 'block-threshold hotspot-section)))
      (define risk-notes-raw (cdr (assoc 'risk-notes hotspot-section)))
      (define risk-note-files
        (for/set ([entry (in-list risk-notes-raw)])
          (car entry)))
      ;; Get current hotspot scores
      (define-values (code out) (run-hotspot-report "--json" "--all"))
      (define data (string->jsexpr out))
      (define hotspots (hash-ref data 'hotspots))
      (define blocking-violations
        (for/list ([h (in-list hotspots)]
                   #:when (> (hash-ref h 'score) block-threshold))
          (define f (hash-ref h 'file))
          (if (set-member? risk-note-files f)
              #f
              (format "~a: score ~a > ~a but no risk note" f (hash-ref h 'score) block-threshold))))
      (define actual-violations (filter identity blocking-violations))
      (check-equal? actual-violations
                    '()
                    (format "Hotspot budget violations: ~a" actual-violations)))))

(run-tests hotspot-tests)
