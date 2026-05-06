#lang racket/base

;; util/shell-quote.rkt — POSIX single-quote shell escaping
;;
;; SEC-16 (v0.22.0): Consolidated from sandbox/subprocess.rkt and
;; extensions/github/helpers.rkt to eliminate duplication.
;;
;; SECURITY NOTE: This function is designed for quoting LLM-generated arguments
;; within a trusted agent loop. It is NOT intended as a defense against
;; adversarial input. The sandbox layer provides defense-in-depth, but callers
;; must not rely on shell-quote alone to prevent injection from untrusted sources.

(require racket/contract
         racket/format
         racket/string)

(provide (contract-out [shell-quote (-> any/c string?)]))

;; POSIX single-quote escaping for command arguments.
;; Converts non-string inputs to string via ~a before quoting.
(define (shell-quote s)
  (define str
    (if (string? s)
        s
        (~a s)))
  (string-append "'" (string-replace str "'" "'\\''") "'"))
