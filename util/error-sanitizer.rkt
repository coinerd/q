#lang racket/base

;; util/error-sanitizer.rkt — Sanitize error messages to remove sensitive paths (SEC-12)
;;
;; Tools should use `sanitize-error-message` before returning error results
;; to prevent leaking full system paths to the LLM.

(require racket/string)

(provide sanitize-error-message)

;; sanitize-error-message : string? -> string?
;; Replaces sensitive content in error messages:
;;   1. Home directory path → ~/
;;   2. API key prefixes (sk-..., ghp_..., xoxb-..., key-...) → [REDACTED]
;;   3. /tmp/ paths → [REDACTED]
;;   4. Email addresses → [REDACTED]
;; Pure function — no side effects.
(define (sanitize-error-message msg)
  (define home (find-system-path 'home-dir))
  (define home-str (path->string home))
  ;; Handle both trailing-slash and no-trailing-slash variants
  (define home-prefix
    (if (string-suffix? home-str "/")
        home-str
        (string-append home-str "/")))
  (define after-home (string-replace msg home-prefix "~/"))
  ;; API key prefixes: sk-..., ghp_..., xoxb-..., key-...
  (define after-api-keys
    (regexp-replace* #rx"(^|[^A-Za-z0-9])(sk-|ghp_|xoxb-|key-)[A-Za-z0-9_./-]+"
                     after-home
                     "\\1[REDACTED]"))
  ;; /tmp/ paths
  (define after-tmp (regexp-replace* #rx"/tmp/[^ \t\n\r\"']*" after-api-keys "[REDACTED]"))
  ;; Email addresses: user@domain.com
  (define after-email
    (regexp-replace* #px"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}" after-tmp "[REDACTED]"))
  after-email)
