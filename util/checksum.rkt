#lang racket/base

;; util/checksum.rkt -- SHA-256 checksum utilities
;;
;; Pure cryptographic utilities. No dependency on q internals.
;; Uses sha256sum subprocess (Racket 8.10 lacks native sha256).

(require racket/contract
         racket/port
         racket/string)

;; ============================================================
;; Provides
;; ============================================================

(provide
 ;; SHA-256 hashing and checksum verification
 (contract-out
  [sha256-string        (-> string? string?)]
  [sha256-file          (-> path-string? string?)]
  [verify-file-checksum (-> path-string? string? boolean?)]))

;; ============================================================
;; SHA-256 via sha256sum subprocess
;; ============================================================

(define (sha256-string input)
  (define-values (sp out-in out-out err-in)
    (subprocess #f #f #f "/bin/sh" "-c" "sha256sum"))
  (display input out-out)
  (close-output-port out-out)
  (define result (port->string out-in))
  (close-input-port out-in)
  (close-input-port err-in)
  (subprocess-wait sp)
  (car (string-split (string-trim result))))

;; ============================================================
;; File checksum
;; ============================================================

(define (sha256-file path)
  (call-with-input-file path
    (lambda (in)
      (sha256-string (port->string in)))
    #:mode 'text))

;; ============================================================
;; File checksum verification
;; ============================================================

(define (verify-file-checksum path expected-hash)
  (string=? (sha256-file path) expected-hash))
