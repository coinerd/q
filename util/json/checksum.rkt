#lang racket/base

;; util/checksum.rkt -- SHA-256 checksum utilities
;;
;; Pure cryptographic utilities. No dependency on q internals.
;; Uses Racket openssl module for SHA-256 (no subprocess).

(require racket/contract
         racket/port
         file/sha1 ; bytes->hex-string
         openssl) ; sha256-bytes, sha256-bytes-stream

;; ============================================================
;; Provides
;; ============================================================

;; SHA-256 hashing and checksum verification
(provide (contract-out [sha256-string (-> string? string?)]
                       [sha256-file (-> path-string? string?)]
                       [verify-file-checksum (-> path-string? string? boolean?)]))

;; ============================================================
;; SHA-256 via Racket openssl (W5.3: replaced shell sha256sum)
;; ============================================================

(define (sha256-string input)
  (bytes->hex-string (sha256-bytes (open-input-string input))))

;; ============================================================
;; File checksum — streamed to avoid loading entire file
;; ============================================================

(define (sha256-file path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

;; ============================================================
;; File checksum verification
;; ============================================================

(define (verify-file-checksum path expected-hash)
  (string=? (sha256-file path) expected-hash))
