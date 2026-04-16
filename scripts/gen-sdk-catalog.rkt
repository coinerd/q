#!/usr/bin/env racket
#lang racket/base

;; gen-sdk-catalog.rkt — Generate SDK/RPC catalog from source provides.
;;
;; Scans key modules for `provide` forms and outputs a structured
;; catalog in Markdown format. Used for release documentation.
;;
;; Usage:
;;   racket scripts/gen-sdk-catalog.rkt          # print to stdout
;;   racket scripts/gen-sdk-catalog.rkt --write   # write to docs/sdk-rpc-catalog.md

(require racket/file
         racket/string
         racket/list
         racket/path
         racket/port)

;; ---------------------------------------------------------------------------
;; Module scanning
;; ---------------------------------------------------------------------------

(define MODULES
  '(("interfaces/sdk.rkt" "SDK Interface" "stable") ("wiring/rpc-methods.rkt" "RPC Methods" "stable")
                                                    ("interfaces/rpc-mode.rkt" "RPC Mode" "evolving")
                                                    ("extensions/api.rkt" "Extension API" "stable")
                                                    ("extensions/hooks.rkt" "Extension Hooks"
                                                                            "evolving")))

(define (extract-provides content)
  ;; Extract identifiers from provide forms
  (define lines (string-split content "\n"))
  (append* (for/list ([line (in-list lines)])
             (define trimmed (string-trim line))
             (cond
               ;; Skip comments
               [(string-prefix? trimmed ";") '()]
               ;; (provide ...) form — extract exported names
               [(string-prefix? trimmed "(provide")
                (define inner (regexp-match #rx"\\(provide\\s+(.*)\\)" trimmed))
                (if inner
                    (let ([body (cadr inner)])
                      (filter (lambda (s) (not (string-prefix? s "#:"))) (string-split body)))
                    '())]
               [else '()]))))

(define (scan-module rel-path)
  (define full-path (build-path (current-directory) rel-path))
  (if (file-exists? full-path)
      (extract-provides (file->string full-path))
      '()))

;; ---------------------------------------------------------------------------
;; Markdown generation
;; ---------------------------------------------------------------------------

(define (generate-catalog)
  (define lines
    (list "# q SDK & RPC Catalog (Auto-Generated)"
          ""
          (format "<!-- generated: ~a -->" (current-seconds))
          ""
          "## Exported Symbols by Module"
          ""))
  (for ([mod (in-list MODULES)])
    (define rel-path (car mod))
    (define title (cadr mod))
    (define tier (caddr mod))
    (define exports (scan-module rel-path))
    (set! lines
          (append lines
                  (list (format "### ~a (`~a`)" title rel-path)
                        ""
                        (format "**Stability**: ~a" tier)
                        ""
                        (if (null? exports)
                            "No exports found."
                            (format "**Exports**: `~a`" (string-join exports "`, `")))
                        ""))))
  (string-join lines "\n"))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define write-mode? (member "--write" args))
  (define catalog (generate-catalog))

  (cond
    [write-mode?
     (define out-path (build-path (current-directory) "docs" "sdk-rpc-catalog-autogen.md"))
     (call-with-output-file out-path (lambda (out) (display catalog out)) #:exists 'truncate)
     (printf "Written to ~a~n" (path->string out-path))]
    [else (displayln catalog)]))

(main)
