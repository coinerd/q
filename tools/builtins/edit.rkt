#lang racket/base

;; tools/builtins/edit.rkt — exact replacement edits with near-match hints
;;
;; Exports:
;;   tool-edit : (hash [exec-ctx]) -> tool-result?
;;   Arguments: path (string), old-text (string), new-text (string)
;;   Returns:  tool-result with success or error details
;;
;; Safeguards (v0.19.10):
;;   - old-text length limit (500 chars) to reduce blast radius
;;   - post-edit line-count delta check with auto-revert on corruption
;;   - pre-edit backup save to ~/.q/edit-backups/

(require racket/contract
         racket/file
         racket/match
         racket/string
         (only-in racket/list last drop)
         (only-in "../tool.rkt" make-success-result make-error-result exec-context? tool-result?)
         (only-in "../../extensions/gsd/session-state.rkt"
                  [current-edit-limit current-max-old-text-len]
                  [set-edit-limit! set-current-max-old-text-len!])
         (only-in "../../util/path-helpers.rkt" expand-home-path)
         (only-in "../../util/error-sanitizer.rkt" sanitize-error-message)
         (only-in "builtin-helpers.rkt" require-safe-path!)
         "edit-normalize.rkt")

(define current-fuzzy-edit-enabled? (make-parameter #f))

(provide current-max-old-text-len
         set-current-max-old-text-len!
         current-fuzzy-edit-enabled?
         (contract-out [tool-edit (->* (hash?) ((or/c exec-context? #f)) tool-result?)]))

;; --------------------------------------------------
;; Backup helpers
;; --------------------------------------------------
;; Constants
;; --------------------------------------------------

(define MAX-BACKUPS-PER-FILE 10)

;; --------------------------------------------------
;; Helpers
;; --------------------------------------------------

(define (ensure-backup-dir)
  (define dir (build-path (find-system-path 'home-dir) ".q" "edit-backups"))
  (unless (directory-exists? dir)
    (make-directory* dir)
    ;; SEC-D: Restrict backup dir to owner only (0700)
    (file-or-directory-permissions dir #o700))
  dir)

(define (save-backup path-str content)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/backup: ~a" (exn-message e)))
                               #f)])
    (define dir (ensure-backup-dir))
    (define basename (file-name-from-path path-str))
    (define timestamp (number->string (abs (current-milliseconds))))
    (define backup-name (format "~a_~a" timestamp basename))
    (define backup-path (build-path dir backup-name))
    (display-to-file content backup-path #:exists 'replace)
    ;; Prune old backups for this basename
    (prune-old-backups dir basename)
    (path->string backup-path)))

(define (file-name-from-path p)
  (define fname
    (if (string? p)
        p
        (path->string p)))
  (define parts (regexp-split #rx"/" fname))
  (if (null? parts)
      "unknown"
      (last parts)))

(define (prune-old-backups dir basename)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/prune: ~a" (exn-message e)))
                               (void))])
    (define all (directory-list dir))
    ;; v0.21.5 (F5): Use suffix match to avoid pruning backups for unrelated
    ;; files whose basename happens to be a substring (e.g. "bar" inside "foobar").
    (define matching
      (filter (lambda (f) (string-suffix? (path->string f) (format "_~a" basename)))
              (sort (map path->string all) string>?)))
    (when (> (length matching) MAX-BACKUPS-PER-FILE)
      (for ([f (in-list (drop matching MAX-BACKUPS-PER-FILE))])
        (delete-file (build-path dir f))))))

;; --------------------------------------------------
;; Line-count helper
;; --------------------------------------------------

(define (line-count s)
  (length (string-split s "\n" #:trim? #f)))

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
           (match (list (+ i di) (+ j dj))
             [(list (? (lambda (x) (or (>= x la))) _) _) (max best len)]
             [(list _ (? (lambda (x) (>= x lb)))) (max best len)]
             [(list (? (lambda (x) (char=? (string-ref a x) (string-ref b (+ j dj))))) _)
              (loop (add1 di) (add1 dj) (add1 len))]
             [_ (max best len)]))))]))

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
  (match key-len
    [0 (values #f #f)]
    [_
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
;; v0.21.2: Enhanced with common-cause hints to speed up recovery.
(define (make-not-found-error path-str old-text content)
  (define-values (line-num line-text) (find-nearest-match content old-text))
  (define hint
    (match old-text
      [(? (lambda (s) (and (string? s) (regexp-match? #rx"^ +" s))))
       "Hint: old-text has leading whitespace — check indentation."]
      [(? (lambda (s)
            (and (string? s) (> (string-length s) 200) (< (length (string-split s "\n")) 2))))
       "Hint: old-text is very long and single-line — try a smaller unique snippet."]
      [_ ""]))
  (match line-num
    [#f
     (string-append (format "old-text not found in ~a. Read the file first to get exact text.\n"
                            path-str)
                    hint)]
    [_
     (string-append
      (format "old-text not found in ~a.\nNearest match at line ~a:\n  \"" path-str line-num)
      (string-trim line-text)
      "\"\nRe-read the file to get exact text.\n"
      hint)]))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define expanded (and raw-path (expand-home-path raw-path)))
  ;; SEC-03 (v0.22.0): Canonicalize path to prevent traversal attacks
  (define path-str
    (and expanded
         (let ([p (if (string? expanded)
                      expanded
                      (path->string expanded))])
           (with-handlers ([exn:fail? (lambda (_) p)])
             (path->string (simplify-path (resolve-path p)))))))
  ;; Argument validation via match — flattened pyramid
  (match (list path-str (hash-ref args 'old-text #f) (hash-ref args 'new-text #f))
    [(list #f _ _) (make-error-result "Missing required argument: path")]
    [(list _ #f _) (make-error-result "Missing required argument: old-text")]
    [(list _ _ #f) (make-error-result "Missing required argument: new-text")]
    [(list (? string? path) (? string? old-text) (? string? new-text))
     (cond
       ;; Defense-in-depth: verify path even if scheduler already checked (SEC-09)
       [(require-safe-path! path "edit")
        =>
        (lambda (err) (make-error-result err))]
       [(not (file-exists? path)) (make-error-result (format "File not found: ~a" path))]
       [else
        (define content (file->string path))
        (match (string-length old-text)
          [(? (lambda (n) (> n (current-max-old-text-len))))
           (make-error-result
            (format
             "old-text is too long (~a chars, max ~a). Break your edit into smaller pieces — each edit should change ≤20 lines."
             (string-length old-text)
             (current-max-old-text-len)))]
          [_
           (define occurrences (count-occurrences content old-text))
           (define fuzzy-allowed?
             (or (hash-ref args 'fuzzy? (current-fuzzy-edit-enabled?)) (current-fuzzy-edit-enabled?)))
           (define fuzzy-match
             (and (zero? occurrences) fuzzy-allowed? (fuzzy-find-match content old-text)))
           (cond
             [(and (zero? occurrences) (not fuzzy-match))
              (make-error-result (make-not-found-error path old-text content))]
             [(> occurrences 1)
              (make-error-result
               (format "old-text appears ~a times in ~a; be more specific" occurrences path))]
             [else
              ;; occurrences = 1 or fuzzy-match succeeded
              (define backup-path (save-backup path content))
              (define new-content
                (if fuzzy-match
                    (string-append (substring content 0 (car fuzzy-match))
                                   new-text
                                   (substring content (cdr fuzzy-match)))
                    (string-replace content old-text new-text #:all? #f)))
              (define expected-delta (- (line-count new-text) (line-count old-text)))
              (define actual-delta (- (line-count new-content) (line-count content)))
              (define delta-diff (abs (- actual-delta expected-delta)))
              (match delta-diff
                [(? (lambda (d) (> d 2)))
                 ;; Auto-revert: write back original content
                 (with-handlers ([exn:fail? (lambda (e)
                                              (log-warning (format "edit/auto-revert: ~a"
                                                                   (exn-message e)))
                                              (void))])
                   (display-to-file content path #:exists 'replace))
                 (make-error-result
                  (format (string-append "Edit reverted: line count changed unexpectedly "
                                         "(expected ~a lines delta, got ~a). "
                                         "The file may have been modified since your last read. "
                                         "Re-read the file and try a smaller edit.")
                          expected-delta
                          actual-delta))]
                [_
                 ;; Write new content
                 (with-handlers ([exn:fail:filesystem?
                                  (lambda (e)
                                    (make-error-result (sanitize-error-message
                                                        (format "Write error: ~a"
                                                                (exn-message e)))))])
                   (call-with-output-file path
                                          (lambda (out) (display new-content out))
                                          #:exists 'replace)
                   (make-success-result (list (format "Edited ~a (replaced ~a occurrence)"
                                                      path
                                                      (if fuzzy-match 1 occurrences)))
                                        (hasheq 'path
                                                path
                                                'replacements
                                                1
                                                'old-length
                                                (string-length old-text)
                                                'new-length
                                                (string-length new-text)
                                                'backup
                                                (or backup-path ""))))])])])])]))
