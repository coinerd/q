#lang racket/base

;; tools/builtins/edit.rkt — exact replacement edits with ambiguity checks
;;
;; Exports:
;;   tool-edit : (hash [exec-ctx]) -> tool-result?
;;   Arguments: path (string), old-text (string), new-text (string)
;;   Returns:  tool-result with success or error details

(require racket/file
         (only-in "../tool.rkt" make-success-result make-error-result))

(provide tool-edit)

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
      +inf.0 ; empty needle is found everywhere
      (let loop ([pos 0]
                 [count 0])
        (define found (str-find haystack needle pos))
        (if found
            (loop (+ found nlen) (add1 count))
            count))))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define path-str (hash-ref args 'path #f))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define old-text (hash-ref args 'old-text #f))
     (cond
       [(not old-text) (make-error-result "Missing required argument: old-text")]
       [else
        (define new-text (hash-ref args 'new-text #f))
        (cond
          [(not new-text) (make-error-result "Missing required argument: new-text")]
          [else
           ;; 1. File existence check
           ;; (safe-mode path check is done by scheduler, not here)
           (cond
             [(not (file-exists? path-str))
              (make-error-result (format "File not found: ~a" path-str))]

             [else
              ;; 2. Read file content
              (define content (file->string path-str))

              ;; 3. Check for ambiguity (multiple matches)
              (define occurrences (count-occurrences content old-text))

              (cond
                [(zero? occurrences) (make-error-result (format "old-text not found in ~a" path-str))]

                [(> occurrences 1)
                 (make-error-result
                  (format "old-text appears ~a times in ~a; be more specific" occurrences path-str))]

                [else
                 ;; 4. Perform replacement
                 (define new-content (regexp-replace (regexp-quote old-text) content new-text))

                 ;; 5. Write back
                 (with-handlers ([exn:fail:filesystem?
                                  (lambda (e)
                                    (make-error-result (format "Write error: ~a" (exn-message e))))])
                   (call-with-output-file path-str
                                          (lambda (out) (display new-content out))
                                          #:exists 'replace)

                   (make-success-result
                    (list (format "Edited ~a (replaced ~a occurrence)" path-str occurrences))
                    (hasheq 'path
                            path-str
                            'replacements
                            1
                            'old-length
                            (string-length old-text)
                            'new-length
                            (string-length new-text))))])])])])]))
