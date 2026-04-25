#lang racket/base

;; tools/builtins/edit.rkt — exact replacement edits with near-match hints
;;
;; Exports:
;;   tool-edit : (hash [exec-ctx]) -> tool-result?
;;   Arguments: path (string), old-text (string), new-text (string)
;;   Returns:  tool-result with success or error details

(require racket/file
         racket/string
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-path?
                  safe-mode-project-root)
         (only-in "../../util/path-helpers.rkt" expand-home-path))

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
;; Near-match helper for better error messages
;; --------------------------------------------------

;; Compute length of longest common substring between two strings.
;; Uses a simple dynamic programming approach, bounded for performance.
(define (longest-common-substring-len a b)
  (define la (string-length a))
  (define lb (string-length b))
  (cond
    [(or (zero? la) (zero? lb)) 0]
    [else
     ;; Use sliding window approach for memory efficiency
     (for/fold ([best 0]) ([i (in-range la)])
       (for/fold ([best best]) ([j (in-range lb)])
         (let loop ([di 0]
                    [dj 0]
                    [len 0])
           (cond
             [(or (>= (+ i di) la) (>= (+ j dj) lb)) (max best len)]
             [(char=? (string-ref a (+ i di)) (string-ref b (+ j dj)))
              (loop (add1 di) (add1 dj) (add1 len))]
             [else (max best len)]))))]))

;; Extract a search key from old-text: trimmed, up to 60 chars.
(define (extract-search-key old-text)
  (define trimmed (string-trim old-text))
  (if (<= (string-length trimmed) 60)
      trimmed
      (substring trimmed 0 60)))

;; Find the nearest matching line in content to old-text.
;; Returns (values line-number line-text) or (values #f #f).
;; line-number is 1-indexed. Only returns if similarity > 40%.
(define (find-nearest-match content old-text)
  (define lines (string-split content "\n" #:trim? #f))
  (define key (extract-search-key old-text))
  (define key-len (string-length key))
  (cond
    [(zero? key-len) (values #f #f)]
    [else
     (for/fold ([best-line #f]
                [best-num #f]
                [best-score 0]
                #:result (if (> best-score (* key-len 0.4))
                             (values best-num best-line)
                             (values #f #f)))
               ([line (in-list lines)]
                [idx (in-naturals 1)])
       (define trimmed (string-trim line))
       (define lcs (longest-common-substring-len key trimmed))
       (if (> lcs best-score)
           (values line idx lcs)
           (values best-line best-num best-score)))]))

;; Build enhanced error message when old-text is not found.
(define (make-not-found-error path-str old-text content)
  (define-values (line-num line-text) (find-nearest-match content old-text))
  (cond
    [line-num
     (string-append
      (format "old-text not found in ~a.\nNearest match at line ~a:\n  \"" path-str line-num)
      (string-trim line-text)
      "\"\nRe-read the file to get exact text.")]
    [else (format "old-text not found in ~a. Read the file first to get exact text." path-str)]))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))
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
           ;; Defense-in-depth: verify path even if scheduler already checked (SEC-09)
           (cond
             [(and (safe-mode?) (not (allowed-path? path-str)))
              (make-error-result (format "edit: path '~a' outside project root (~a)"
                                         path-str
                                         (safe-mode-project-root)))]
             [(not (file-exists? path-str))
              (make-error-result (format "File not found: ~a" path-str))]

             [else
              ;; Read file content
              (define content (file->string path-str))

              ;; Check for ambiguity (multiple matches)
              (define occurrences (count-occurrences content old-text))

              (cond
                [(zero? occurrences)
                 (make-error-result (make-not-found-error path-str old-text content))]

                [(> occurrences 1)
                 (make-error-result
                  (format "old-text appears ~a times in ~a; be more specific" occurrences path-str))]

                [else
                 ;; Perform replacement
                 (define new-content (regexp-replace (regexp-quote old-text) content new-text))

                 ;; Write back
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
