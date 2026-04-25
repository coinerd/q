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
;;;   truncate-output-with-overflow — truncate + save full output to temp file
;;;   output-overflow-dir — parameter for overflow temp-file directory
;;;
;;; #774, #1115, G5.2

(require racket/string
         racket/match
         racket/file
         racket/list
         racket/path)

(provide MAX-OUTPUT-BYTES
         MAX-OUTPUT-LINES
         MAX-OUTPUT-CHARS
         truncate-output
         truncate-to-n-lines
         truncate-to-n-chars
         output-exceeds-limits?
         truncate-output-with-overflow
         output-overflow-dir)

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

;; -------------------------------------------------------------------
;;; G5.2 — Tool Output Truncation with temp-file overflow
;;; -------------------------------------------------------------------

;; Parameter for overflow directory (overridable for testing)
(define output-overflow-dir
  (make-parameter (build-path (find-system-path 'home-dir) ".q" "output-overflow")))

;; Maximum overflow files to keep per basename
(define MAX-OVERFLOW-FILES-PER-BASE 10)

;; Rotate overflow files for a given basename: keep only the newest N.
;; path-string? -> void?
(define (rotate-overflow-files base-name)
  (define dir (output-overflow-dir))
  (when (directory-exists? dir)
    (define prefix (string-append base-name "-"))
    (define candidates
      (filter (λ (f)
                (define name (path->string (file-name-from-path f)))
                (string-prefix? name prefix))
              (directory-list dir)))
    (when (> (length candidates) MAX-OVERFLOW-FILES-PER-BASE)
      (define sorted
        (sort candidates > #:key (λ (f) (file-or-directory-modify-seconds (build-path dir f)))))
      (for ([f (drop sorted MAX-OVERFLOW-FILES-PER-BASE)])
        (delete-file (build-path dir f))))))

;; Generate a unique overflow file path for the given base name.
;; string? -> path?
(define (make-overflow-path base-name)
  (define dir (output-overflow-dir))
  (make-directory* dir)
  (define ts (number->string (inexact->exact (floor (current-inexact-milliseconds)))))
  (define fname (string-append base-name "-" ts ".txt"))
  (build-path dir fname))

;; Truncate output, saving full content to a temp file when limits are exceeded.
;; string? [string?] -> string?
;;; When output exceeds limits, saves the FULL output to a temp file in
;;; (output-overflow-dir), truncates the returned string as truncate-output
;;; would, and appends a notice with the temp file path and total stats.
(define (truncate-output-with-overflow str [base-name "output"])
  (define byte-count (string-length str))
  (define line-count (count-lines str))
  (cond
    [(and (<= byte-count MAX-OUTPUT-BYTES) (<= line-count MAX-OUTPUT-LINES)) str]
    [else
     ;; Save full output to overflow file
     (define overflow-path (make-overflow-path base-name))
     (call-with-output-file overflow-path (λ (out) (display-string str out)) #:exists 'truncate)
     ;; Rotate old files
     (rotate-overflow-files base-name)
     ;; Truncate as normal
     (define truncated (truncate-output str))
     ;; Append overflow notice
     (define-values (total-label total-amount)
       (if (> line-count MAX-OUTPUT-LINES)
           (values "lines" line-count)
           (values "bytes" byte-count)))
     (define notice
       (format "\n[Full output saved to ~a — ~a total ~a]"
               (path->string overflow-path)
               total-amount
               total-label))
     (string-append truncated notice)]))

;; display-string helper (Racket doesn't have a display-string that writes
;; to a port directly in racket/base)
(define (display-string str out)
  (write-string str out))
