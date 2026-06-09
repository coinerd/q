#lang racket/base

;; q/scripts/run-tests/inventory.rkt — Inventory report mode
;;
;; Prints selected/excluded files with classifier hits and risk flags.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string
         racket/path
         (only-in "classify.rkt"
                  base-dir
                  slow-file?
                  tui-file?
                  security-file?
                  mutating-file?
                  arch-file?
                  runtime-file?
                  extensions-file?
                  workflows-file?
                  support-test-module?))

(provide print-inventory
         classify-exclusion-reason
         detect-high-risk-flags
         compute-inventory-hash)

(define (classify-exclusion-reason f)
  (cond
    [(string-contains? f "/compiled/") 'compiled]
    [(not (string-suffix? f ".rkt")) 'non-rkt]
    [(support-test-module? f) 'support-module]
    [else 'unknown]))

(define (detect-high-risk-flags f)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define resolved
      (if (absolute-path? f)
          f
          (build-path base-dir f)))
    (define content (file->string resolved))
    (define flags '())
    (when (regexp-match? #rx"current-directory" content)
      (set! flags (cons 'cwd flags)))
    (when (regexp-match? #rx"getenv" content)
      (set! flags (cons 'env flags)))
    (when (regexp-match? #rx"make-temporary" content)
      (set! flags (cons 'temp-file flags)))
    (when (regexp-match? #rx"subprocess" content)
      (set! flags (cons 'subprocess flags)))
    (when (or (regexp-match? #rx"benchmark" content) (regexp-match? #rx"perf" content))
      (set! flags (cons 'perf flags)))
    (when (regexp-match? #rx"terminal" content)
      (set! flags (cons 'terminal flags)))
    (reverse flags)))

(define (compute-inventory-hash files)
  (define content (string-join (sort files string<?)))
  (define bytes (string->bytes/utf-8 content))
  (format "~x" (equal-hash-code bytes)))

(define (list->set lst)
  (for/hash ([x (in-list lst)])
    (values x #t)))

(define (set-member? st k)
  (hash-has-key? st k))

(define (print-inventory suite suite-files)
  (define all-rkt-files
    (for/list ([f (in-directory (build-path base-dir "tests"))]
               #:when (and (file-exists? f)
                           (string-suffix? (path->string f) ".rkt")
                           (not (string-contains? (path->string f) "/compiled/"))))
      (path->string (find-relative-path base-dir f))))
  (define suite-set (list->set suite-files))
  (define excluded (filter (lambda (f) (not (set-member? suite-set f))) all-rkt-files))
  (printf ";; INVENTORY REPORT — suite: ~a~n" suite)
  (printf ";; ═══════════════════════════════════════~n")
  (printf ";; Selected files: ~a~n" (length suite-files))
  (printf ";; Excluded files: ~a~n" (length excluded))
  (printf ";; Inventory hash: ~a~n~n" (compute-inventory-hash suite-files))
  (printf ";; SELECTED FILES:~n")
  (for ([f (in-list (sort suite-files string<?))])
    (define resolved
      (if (absolute-path? f)
          f
          (build-path base-dir f)))
    (define flags
      (if (file-exists? resolved)
          (detect-high-risk-flags f)
          '()))
    (define suite-hits
      (filter values
              (list (and (slow-file? f) 'slow)
                    (and (tui-file? f) 'tui)
                    (and (security-file? f) 'security)
                    (and (mutating-file? f) 'mutating)
                    (and (arch-file? f) 'arch)
                    (and (runtime-file? f) 'runtime)
                    (and (extensions-file? f) 'extensions)
                    (and (workflows-file? f) 'workflows))))
    (when (pair? flags)
      (printf "  ~a  [high-risk: ~a]~n" f (string-join (map symbol->string flags) ",")))
    (when (pair? suite-hits)
      (printf "    classifiers: ~a~n" (string-join (map symbol->string suite-hits) ","))))
  (when (pair? excluded)
    (newline)
    (printf ";; EXCLUDED FILES:~n")
    (for ([f (in-list (sort excluded string<?))])
      (define reason (classify-exclusion-reason f))
      (printf "  ~a  [reason: ~a]~n" f reason))))

(require racket/file
         racket/list)
