#lang racket/base

;; util/path-helpers.rkt — shared path manipulation helpers
;;
;; Extracted from tools/builtins/write.rkt and extensions/loader.rkt
;; to eliminate duplication (QUAL-02).

(require racket/list
         racket/string)

(provide ;; Path utility
 path-only
 expand-home-path
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

;; Expand leading ~ in a path string to the user's home directory.
;; If the path doesn't start with ~, returns it unchanged.
;; This is needed because Racket's file-exists?, directory-exists?, etc.
;; do NOT expand ~ — they treat it as a literal directory name.
(define (expand-home-path path-str)
  (if (and (string? path-str)
           (> (string-length path-str) 0)
           (char=? (string-ref path-str 0) #\~))
      (let ([home (find-system-path 'home-dir)])
        (if (and (> (string-length path-str) 1)
                 (char=? (string-ref path-str 1) #\/))
            (string-append (path->string home) (substring path-str 1))
            (path->string home)))
      path-str))
