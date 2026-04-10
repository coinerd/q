#lang racket/base

;; q/cli/export.rkt — Top-level session export dispatcher
;;
;; Provides:
;;   export-session       — path-string? symbol? -> string?
;;   export-session-to-file — path-string? symbol? path-string? -> void?

(require racket/contract
         racket/file
         "../runtime/session-store.rkt"
         "../util/export-markdown.rkt"
         "../util/export-html.rkt"
         "../util/export-json.rkt")

(provide export-session
         export-session-to-file)

;; ── Helpers ──

(define (ensure-parent-dirs path)
  (define-values (dir name must-be-dir?) (split-path path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir)))

;; ── Dispatcher ──

(define (export-session session-path format)
  ;; Load session from path and export in the given format.
  ;; format: 'markdown, 'html, or 'json
  ;; Returns the exported string.
  (define messages (load-session-log session-path))
  (case format
    [(markdown) (session->markdown messages)]
    [(html) (session->html messages)]
    [(json) (session->json-string messages)]
    [else (error 'export-session "unknown format: ~a" format)]))

(define (export-session-to-file session-path format output-path)
  ;; Export session and write result to output-path.
  ;; Creates parent directories if needed.
  (ensure-parent-dirs output-path)
  (define result (export-session session-path format))
  (call-with-output-file output-path
    (lambda (out)
      (display result out))
    #:mode 'text
    #:exists 'truncate))
