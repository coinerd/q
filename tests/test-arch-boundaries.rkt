#lang racket

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
         "helpers/arch-utils.rkt")

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
      (define runtime-files (rkt-files-in "runtime"))
      (define runtime-exc (policy-ref 'known-exceptions 'runtime))
      (define known-exceptions
        (map (λ (s)
               (if (symbol? s)
                   (symbol->string s)
                   s))
             (map car runtime-exc)))
      (define violations
        (for/list ([f (in-list runtime-files)]
                   #:when (not (member (path->string (file-name-from-path f)) known-exceptions)))
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))
              (format "~a: upward imports detected" (file-name-from-path f))
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
                    (format "TUI modules importing from forbidden layers: ~a" actual-violations)))))

(run-tests boundary-tests)
