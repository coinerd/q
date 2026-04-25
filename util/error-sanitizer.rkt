#lang racket/base

;; util/error-sanitizer.rkt — Sanitize error messages to remove sensitive paths (SEC-12)
;;
;; Tools should use `sanitize-error-message` before returning error results
;; to prevent leaking full system paths to the LLM.

(require racket/string)

(provide sanitize-error-message)

;; sanitize-error-message : string? -> string?
;; Replaces the user's home directory path with ~ in error messages.
(define (sanitize-error-message msg)
  (define home (find-system-path 'home-dir))
  (define home-str (path->string home))
  ;; Handle both trailing-slash and no-trailing-slash variants
  (define home-prefix (if (string-suffix? home-str "/")
                          home-str
                          (string-append home-str "/")))
  (string-replace msg home-prefix "~/"))
