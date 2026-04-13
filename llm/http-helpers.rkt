#lang racket/base

;; llm/http-helpers.rkt — Shared HTTP error handling utilities
;;
;; Common helpers for parsing HTTP status lines and raising errors.
;; Used by anthropic.rkt, gemini.rkt, and openai-compatible.rkt.

(require racket/contract)

(provide extract-status-code
         http-error?
         raise-http-error!)

;; ============================================================
;; Contracts
;; ============================================================

(define status-line/c (or/c bytes? string?))

(define response-body/c (or/c bytes? string?))

;; ============================================================
;; Helpers
;; ============================================================

;; Parse "HTTP/N.N NNN ..." → integer status code.
;; Returns 0 if the pattern cannot be matched.
(define (extract-status-code status-line)
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (define m (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str))
  (if m
      (string->number (cadr m))
      0))

;; Returns #t when status-code indicates a client or server error (>= 400).
(define (http-error? status-code)
  (>= status-code 400))

;; Raise exn:fail with a formatted HTTP error message.
(define (raise-http-error! message)
  (raise (exn:fail message (current-continuation-marks))))
