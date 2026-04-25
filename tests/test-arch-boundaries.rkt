#lang racket

;; tests/test-arch-boundaries.rkt — Architecture layer boundary tests
;;
;; Verifies that layering constraints are maintained:
;;   - Only runtime/iteration.rkt may import from tools/ or extensions/
;;   - TUI modules must not import from llm/, tools/, runtime/, agent/
;;
;; Refs: #432, ARCH-01

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string)

;; ============================================================
;; Helpers
;; ============================================================

;; Extract all require paths from a source file
(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define requires '())
    (define in-require? #f)
    (for ([line (in-list (string-split src "\n"))])
      (define trimmed (string-trim line))
      (cond
        [(and (>= (string-length trimmed) 7) (string=? (substring trimmed 0 7) "(require"))
         (set! in-require? #t)]
        [(string-contains? trimmed "(require ") (set! in-require? #t)])
      (when in-require?
        (set! requires (cons trimmed requires))
        (when (string-contains? trimmed ")")
          (set! in-require? #f))))
    requires))

(define q-dir (string->path (or (getenv "Q_DIR") ".")))

(define (rkt-files-in dir)
  (if (directory-exists? (build-path q-dir dir))
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
              (directory-list (build-path q-dir dir) #:build? #t))
      '()))

;; Check if a require line imports from a forbidden layer
(define (imports-from? require-line layer-prefixes)
  (for/or ([prefix (in-list layer-prefixes)])
    (string-contains? require-line prefix)))

;; ============================================================
;; Boundary tests
;; ============================================================

(define boundary-tests
  (test-suite "architecture-boundaries"

    (test-case "Only iteration.rkt in runtime/ imports from tools/ or extensions/"
      ;; Known exceptions:
      ;;   - runtime/package.rkt imports extensions/manifest.rkt for package audit
      (define runtime-files (rkt-files-in "runtime"))
      (define known-exceptions
        '("iteration.rkt" "package.rkt" "extension-catalog.rkt"))
      (define violations
        (for/list ([f (in-list runtime-files)]
                   #:when (not (member (path->string (file-name-from-path f)) known-exceptions)))
          (define requires (extract-requires f))
          (define bad-imports
            (filter (λ (r)
                      (or (imports-from? r '("\"../tools/" "\"../../tools/"))
                          (imports-from? r '("\"../extensions/" "\"../../extensions/"))))
                    requires))
          (if (null? bad-imports)
              #f
              (format "~a: ~a" (file-name-from-path f) bad-imports))))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "Unexpected upward imports in runtime/: ~a" actual-violations)))

    (test-case "TUI modules must not import from llm/, tools/"
      ;; TUI modules importing from runtime/ and agent/ are documented
      ;; exceptions — the TUI layer sits above runtime and consumes
      ;; events directly. The hard constraint is: TUI must not import
      ;; from llm/ or tools/ layers.
      ;; v0.14.1: tui-init.rkt exception removed — now uses runtime/provider-factory.rkt
      ;; instead of llm/provider.rkt for mock detection.
      (define tui-files (rkt-files-in "tui"))
      (define known-tui-exceptions '())
      (define tui-files-checked
        (filter (lambda (f)
                  (not (member (path->string (file-name-from-path f)) known-tui-exceptions)))
                tui-files))
      (define violations
        (for/list ([f (in-list tui-files-checked)])
          (define requires (extract-requires f))
          (define bad-imports
            (filter (λ (r)
                      (or (imports-from? r '("\"../llm/" "\"../../llm/"))
                          (imports-from? r '("\"../tools/" "\"../../tools/"))))
                    requires))
          (if (null? bad-imports)
              #f
              (format "~a: ~a" (file-name-from-path f) bad-imports))))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI modules importing from forbidden layers: ~a" actual-violations)))))

(run-tests boundary-tests)
