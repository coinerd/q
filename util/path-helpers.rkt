#lang racket/base

;; util/path-helpers.rkt — shared path manipulation helpers
;;
;; Extracted from tools/builtins/write.rkt and extensions/loader.rkt
;; to eliminate duplication (QUAL-02).

(require racket/list
         racket/string)

(provide ;; Path utility
 path-only
 ;; Byte helpers
 contains-null-bytes?
 bytes->display-lines)

;; Extract directory portion of a path string.
;; Returns #f when the path has no directory component (i.e. is relative/simple).
;; Check if a byte string contains null bytes (binary indicator)
(define (contains-null-bytes? bs)
  (for/or ([b (in-bytes bs)])
    (= b 0)))

;; Convert raw bytes to display lines: decode as UTF-8, split on newlines,
;; trim trailing empty line from trailing newline.
;; Returns (values display-lines total-count).
(define (bytes->display-lines raw-bytes)
  (define text (bytes->string/utf-8 raw-bytes #\?))
  (define all-lines (string-split text "\n" #:trim? #f))
  (define has-trailing-newline
    (and (> (string-length text) 0)
         (char=? (string-ref text (sub1 (string-length text))) #\newline)))
  (define display-lines
    (if has-trailing-newline
        (drop-right all-lines 1)
        all-lines))
  (values display-lines (length display-lines)))

(define (path-only p)
  (define-values (dir _base _must-be-dir?) (split-path p))
  (if (eq? dir 'relative) #f dir))
