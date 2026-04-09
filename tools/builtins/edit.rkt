#lang racket/base

;; tools/builtins/edit.rkt — exact replacement edits with ambiguity checks
;;
;; Exports:
;;   tool-edit : (hash [exec-ctx]) -> tool-result?
;;   Arguments: path (string), old-text (string), new-text (string)
;;   Returns:  tool-result with success or error details

(require racket/file
         (only-in "../tool.rkt"
                  make-success-result make-error-result))

(provide tool-edit)

;; --------------------------------------------------
;; Result helpers (local) - return tool-result structs
;; --------------------------------------------------

(define (ok content details)
  (make-success-result content details))

(define (err msg)
  (make-error-result msg))

;; --------------------------------------------------
;; String helpers
;; --------------------------------------------------

;; Find position of needle in haystack starting from index start
(define (str-find haystack needle [start 0])
  (define sub (substring haystack start))
  (define m (regexp-match-positions (regexp-quote needle) sub))
  (and m (+ start (caar m))))

;; Count non-overlapping occurrences of needle in haystack
(define (count-occurrences haystack needle)
  (define nlen (string-length needle))
  (if (zero? nlen)
      +inf.0  ; empty needle is found everywhere
      (let loop ([pos 0] [count 0])
        (define found (str-find haystack needle pos))
        (if found
            (loop (+ found nlen) (add1 count))
            count))))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define path-str (hash-ref args 'path))
  (define old-text (hash-ref args 'old-text))
  (define new-text (hash-ref args 'new-text))

  ;; 1. File existence check
  (cond
    [(not (file-exists? path-str))
     (err (format "File not found: ~a" path-str))]

    [else
     ;; 2. Read file content
     (define content (file->string path-str))

     ;; 3. Check for ambiguity (multiple matches)
     (define occurrences (count-occurrences content old-text))

     (cond
       [(zero? occurrences)
        (err (format "old-text not found in ~a" path-str))]

       [(> occurrences 1)
        (err (format "old-text appears ~a times in ~a; be more specific"
                     occurrences path-str))]

       [else
        ;; 4. Perform replacement
        (define new-content
          (regexp-replace (regexp-quote old-text) content new-text))

        ;; 5. Write back
        (with-handlers
            ([exn:fail:filesystem?
              (lambda (e)
                (err (format "Write error: ~a" (exn-message e))))])
          (call-with-output-file path-str
            (lambda (out)
              (display new-content out))
            #:exists 'replace)

          (ok (list (format "Edited ~a (replaced ~a occurrence)" path-str occurrences))
              (hasheq 'path path-str
                      'replacements 1
                      'old-length (string-length old-text)
                      'new-length (string-length new-text))))])]))
