#lang racket
;; @speed fast  ;; @suite arch
;; BOUNDARY: integration
;; tests/test-arch-composition-root.rkt — Composition root boundary tests (R-2)
;;
;; Verifies that composition root files (agent-session.rkt, session-lifecycle.rkt)
;; don't exceed reasonable cross-layer import budgets beyond documented exceptions.
;; Refs: #8850, R-2

(require rackunit
         rackunit/text-ui
         "helpers/arch-utils.rkt"
         racket/file
         racket/list
         racket/string)

;; ── Composition root files ────────────────────────────────────────

(define composition-roots '("runtime/agent-session.rkt" "runtime/session/session-lifecycle.rkt"))

;; ══════════════════════════════════════════════════════════════════
;; R-2: Cross-layer import budget
;; ══════════════════════════════════════════════════════════════════
;; Composition roots may import from other layers, but the total
;; number of cross-layer imports (outside runtime/ and util/)
;; should be bounded and tracked.

(define (extract-require-paths filepath)
  ;; Return flat list of module path strings from require specs
  (define specs (extract-requires filepath))
  (apply append
         (for/list ([spec (in-list specs)])
           (cond
             [(string? spec) (list spec)]
             [(path? spec) (list (path->string spec))]
             [(symbol? spec) (list (symbol->string spec))]
             [(and (list? spec) (pair? spec))
              ;; Handle (only-in "path" ...), (except-in "path" ...), etc.
              (define path-part (findf (lambda (x) (or (string? x) (path? x))) spec))
              (if path-part
                  (list (if (path? path-part)
                            (path->string path-part)
                            path-part))
                  '())]
             [else '()]))))

(define (cross-layer-imports filepath)
  ;; Count requires outside runtime/ and util/
  (define paths (extract-require-paths filepath))
  (for/sum
   ([p (in-list paths)])
   (cond
     [(regexp-match? #rx"^\\.\\./(agent|llm|browser|tools|extensions|tui|gui|skills)/" p) 1]
     [(regexp-match? #rx"^\\.\\./\\.\\./(agent|llm|browser|tools|extensions|tui|gui|skills)/" p) 1]
     [else 0])))

(define (cross-layer-import-details filepath)
  (define paths (extract-require-paths filepath))
  (filter
   (lambda (p)
     (or (regexp-match? #rx"^\\.\\./(agent|llm|browser|tools|extensions|tui|gui|skills)/" p)
         (regexp-match? #rx"^\\.\\./\\.\\./(agent|llm|browser|tools|extensions|tui|gui|skills)/" p)))
   (remove-duplicates paths)))

;; ── Tests ─────────────────────────────────────────────────────────

(define composition-root-tests
  (test-suite "Composition root cross-layer import budget (R-2)"

    (for ([root (in-list composition-roots)])
      (define full-path (build-path q-dir root))
      (define count (cross-layer-imports full-path))
      (define details (cross-layer-import-details full-path))

      (printf "~a: ~a cross-layer imports~n" root count)
      (for ([d (in-list details)])
        (printf "  - ~a~n" d))

      (test-case (format "~a cross-layer import budget" root)
        ;; Budget: ≤15 cross-layer imports per composition root
        ;; This is a tracking test, not a strict gate — it documents the current state.
        (check <=
               count
               15
               (format
                "~a has ~a cross-layer imports (budget: 15). Consider extracting to wiring module."
                root
                count))))))

(define all-tests
  (test-suite "Architecture composition root tests (R-2)"
    composition-root-tests))

(module+ main
  (run-tests all-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
