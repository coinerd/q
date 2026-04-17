#lang racket/base

;;; util/truncation.rkt — standardized output truncation utilities
;;;
;;; Provides:
;;;   MAX-OUTPUT-BYTES  — maximum output size in bytes (50KB)
;;;   MAX-OUTPUT-LINES  — maximum number of output lines (2000)
;;;   MAX-OUTPUT-CHARS  — maximum output size in characters (50K)
;;;   truncate-output   — truncate to limits with notice
;;;   truncate-to-n-lines — keep first N lines with notice
;;;   truncate-to-n-chars — keep first N chars with notice
;;;   output-exceeds-limits? — check if output exceeds limits
;;;
;;; #774, #1115

(require racket/string
         racket/match)

(provide MAX-OUTPUT-BYTES
         MAX-OUTPUT-LINES
         MAX-OUTPUT-CHARS
         truncate-output
         truncate-to-n-lines
         truncate-to-n-chars
         output-exceeds-limits?)

;; Constants
(define MAX-OUTPUT-BYTES 50000) ; 50KB
(define MAX-OUTPUT-LINES 2000)
(define MAX-OUTPUT-CHARS 50000) ; 50K characters

;; Check if output exceeds limits
;; string? -> boolean?
(define (output-exceeds-limits? str)
  (or (> (string-length str) MAX-OUTPUT-BYTES) (> (count-lines str) MAX-OUTPUT-LINES)))

;; Truncate output to standard limits.
;; If within limits, returns str unchanged.
;; Otherwise truncates and appends notice.
;; string? -> string?
(define (truncate-output str)
  (define byte-count (string-length str))
  (define line-count (count-lines str))
  (cond
    [(and (<= byte-count MAX-OUTPUT-BYTES) (<= line-count MAX-OUTPUT-LINES)) str]
    [(> line-count MAX-OUTPUT-LINES) (truncate-to-n-lines str MAX-OUTPUT-LINES)]
    [else
     ;; Over byte limit but within line limit — truncate bytes
     (define truncated (substring str 0 MAX-OUTPUT-BYTES))
     (define omitted (- byte-count MAX-OUTPUT-BYTES))
     (string-append truncated (format "\n[Output truncated. ~a bytes omitted]" omitted))]))

;; Truncate to first N lines with notice.
;; string? exact-nonnegative-integer? -> string?
(define (truncate-to-n-lines str n)
  (define lines (string-split str "\n" #:trim? #f))
  (define total-lines (length lines))
  (if (<= total-lines n)
      str
      (let* ([kept (take lines n)]
             [omitted (- total-lines n)]
             [result (string-join kept "\n")])
        (string-append result (format "\n[Output truncated. ~a lines omitted]" omitted)))))

;; Truncate to first N characters with notice.
;; string? exact-nonnegative-integer? -> string?
(define (truncate-to-n-chars str max-chars)
  (if (<= (string-length str) max-chars)
      str
      (string-append (substring str 0 max-chars) "\n...(truncated)")))

;; Count lines in a string
(define (count-lines str)
  (add1 (length (string-split str "\n" #:trim? #f))))

;; require take from racket/list
(require racket/list)
