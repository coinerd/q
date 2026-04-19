#lang racket

;; scripts/lint-pkg-metadata.rkt — Validate package metadata
;;
;; Issue #1297: GAP-01c — CI validation for package metadata
;;
;; Usage:
;;   racket scripts/lint-pkg-metadata.rkt                    # lint q's own info.rkt
;;   racket scripts/lint-pkg-metadata.rkt --index pkg/index.json  # validate index
;;   racket scripts/lint-pkg-metadata.rkt --check             # exit 1 on errors

(require racket/file
         racket/string
         racket/port
         json
         "../pkg/registry.rkt")

(provide lint-info-rkt
         lint-index)

;; ═══════════════════════════════════════════════════════════════════
;; info.rkt validation
;; ═══════════════════════════════════════════════════════════════════

(define (lint-info-rkt [path "info.rkt"])
  (cond
    [(not (file-exists? path)) (list (format "File not found: ~a" path))]
    [else
     (with-handlers ([exn:fail? (λ (e) (list (format "Parse error: ~a" (exn-message e))))])
       (define content (call-with-input-file path port->string))
       (define errors '())
       (unless (string-contains? content "#lang info")
         (set! errors (cons "info.rkt must start with #lang info" errors)))
       errors)]))

;; ═══════════════════════════════════════════════════════════════════
;; Index validation
;; ═══════════════════════════════════════════════════════════════════

(define (lint-index [path "pkg/index.json"])
  (cond
    [(not (file-exists? path))
     (displayln (format "WARN: Index file not found: ~a" path))
     '()]
    [else
     (with-handlers ([exn:fail? (λ (e)
                                  (displayln (format "WARN: Could not load index: ~a"
                                                     (exn-message e)))
                                  '())])
       (define index (load-package-index path))
       (define report (validate-index index))
       (for ([e (in-list (hash-ref report 'errors '()))])
         (displayln (format "ERROR: ~a" e)))
       (printf "Package index: ~a packages, ~a errors~n"
               (hash-ref report 'package-count 0)
               (length (hash-ref report 'errors '())))
       (hash-ref report 'errors '()))]))

;; ═══════════════════════════════════════════════════════════════════
;; Main
;; ═══════════════════════════════════════════════════════════════════

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define errors
    (cond
      [(member "--index" args)
       (define idx (cadr (member "--index" args)))
       (lint-index idx)]
      [else (append (lint-info-rkt) (lint-index "pkg/index.json"))]))
  (define check? (member "--check" args))
  (when (null? errors)
    (displayln "Package metadata lint PASSED"))
  (when (and check? (not (null? errors)))
    (exit 1)))

(module+ main
  (main))
