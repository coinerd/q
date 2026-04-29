#lang racket

;; tests/test-arch-boundaries.rkt — Architecture layer boundary tests
;;
;; Verifies that layering constraints are maintained:
;;   - Only known exceptions in runtime/ may import from tools/ or extensions/
;;   - TUI modules must not import from llm/, tools/
;;
;; Refs: #432, ARCH-01

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string)

;; ============================================================
;; Helpers (read-based S-expression parser)
;; ============================================================

;; Extract all require-spec sub-forms from a source file.
(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define lines (string-split src "\n"))
    (define rest
      (string-join (if (string-prefix? (car lines) "#lang")
                       (cdr lines)
                       lines)
                   "\n"))
    (define forms (port->list read (open-input-string rest)))
    (append* (for/list ([form forms])
               (cond
                 [(and (pair? form) (eq? (car form) 'require)) (cdr form)]
                 [else '()])))))

(define (require-spec->paths spec)
  (cond
    [(string? spec) (list spec)]
    [(symbol? spec) '()]
    [(pair? spec)
     (case (car spec)
       [(only-in prefix-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (list (cadr spec))
            '())]
       [else (append* (map require-spec->paths (cdr spec)))])]
    [else '()]))

(define (imports-from? req-specs layer-prefixes)
  (for*/or ([spec (in-list req-specs)]
            [path (in-list (require-spec->paths spec))])
    (for/or ([prefix (in-list layer-prefixes)])
      (string-contains? path prefix))))

(define q-dir (string->path (or (getenv "Q_DIR") ".")))

(define (rkt-files-in dir)
  (if (directory-exists? (build-path q-dir dir))
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
              (directory-list (build-path q-dir dir) #:build? #t))
      '()))

;; ============================================================
;; Boundary tests
;; ============================================================

(define boundary-tests
  (test-suite "architecture-boundaries"

    (test-case "Only known exceptions in runtime/ import from tools/ or extensions/"
      ;; Known exceptions:
      ;;   - runtime/turn-orchestrator.rkt — imports from tools/ for tool execution
      ;;   - runtime/package.rkt — imports extensions/manifest.rkt for package audit
      ;;   - runtime/extension-catalog.rkt — imports from extensions/
      (define runtime-files (rkt-files-in "runtime"))
      (define known-exceptions '("turn-orchestrator.rkt" "package.rkt" "extension-catalog.rkt"))
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
