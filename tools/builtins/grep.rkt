#lang racket/base

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/path
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/glob.rkt" glob->regexp)
         (only-in "../../util/path-helpers.rkt"
                  contains-null-bytes?
                  bytes->display-lines
                  expand-home-path)
         (only-in "../../util/path-filters.rkt" hidden-name? should-skip-entry? skip-dirs))

(provide tool-grep)

;; ============================================================
;; Defaults
;; ============================================================

(define DEFAULT-GLOB "*")
(define DEFAULT-CASE-INSENSITIVE #f)
(define DEFAULT-MAX-RESULTS 50)
(define DEFAULT-CONTEXT-LINES 2)

;; ============================================================
;; Internal helpers
;; ============================================================

;; Check if any component of the path is hidden or a VCS dir
(define (should-skip-path? p)
  (define parts (explode-path p))
  (for/or ([part (in-list parts)])
    (define s (path->string part))
    (or (hidden-name? s) (member s skip-dirs))))

;; Compile the regex pattern
(define (compile-pattern pattern case-insensitive?)
  (if case-insensitive?
      (regexp (string-append "(?i:" pattern ")"))
      (regexp pattern)))

;; ============================================================
;; Core search in a single file
;; ============================================================

;; Returns (values matched-lines file-error?)
;; matched-lines : (listof (list line-number line-text))
;; file-error? : #t if binary or unreadable (skip silently)
(define (search-file file-path pattern-rx)
  (cond
    [(not (file-exists? file-path)) (values '() #f)]
    [else
     (define raw-bytes
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning (format "grep: ~a" (exn-message e)))
                                    #f)])
         (file->bytes file-path)))
     (cond
       [(not raw-bytes) (values '() #t)] ; unreadable → skip
       [(contains-null-bytes? raw-bytes) (values '() #t)] ; binary → skip
       [else
        (define text (bytes->string/utf-8 raw-bytes #\?))
        (define all-lines (string-split text "\n" #:trim? #f))
        ;; Trim trailing empty element from trailing newline
        (define has-trailing-newline
          (and (> (string-length text) 0)
               (char=? (string-ref text (sub1 (string-length text))) #\newline)))
        (define display-lines
          (if has-trailing-newline
              (drop-right all-lines 1)
              all-lines))
        (define matches
          (for/list ([i (in-naturals 1)]
                     [line (in-list display-lines)]
                     #:when (regexp-match? pattern-rx line))
            (list i line)))
        (values matches #f)])]))

;; ============================================================
;; Format matches with context lines
;; ============================================================

;; Format a single match with surrounding context.
;; file-str : string path
;; match-line-num : 1-indexed line number
;; match-line-text : the matched line text
;; all-file-lines : vector of all lines in the file (0-indexed)
;; context-lines : number of context lines before/after
(define (format-match-with-context file-str
                                   match-line-num
                                   match-line-text
                                   all-file-lines-vec
                                   context-lines)
  (define zero-idx (sub1 match-line-num))
  (define start-idx (max 0 (- zero-idx context-lines)))
  (define end-idx (min (sub1 (vector-length all-file-lines-vec)) (+ zero-idx context-lines)))
  (for/list ([i (in-range start-idx (add1 end-idx))])
    (define line-num (add1 i))
    (define line-text (vector-ref all-file-lines-vec i))
    (format "~a:~a: ~a" file-str line-num line-text)))

;; ============================================================
;; Collect files to search
;; ============================================================

;; Glob conversion now lives in util/glob.rkt (glob->regexp)

;; Recursively collect files under dir matching glob-pattern,
;; skipping hidden and VCS dirs.
(define (collect-files dir-path-str glob-pattern)
  (define dir-path (string->path dir-path-str))
  ;; Use in-directory for recursive walk
  (define all-paths
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning (format "grep: ~a" (exn-message e)))
                                 '())])
      (for/list ([p (in-directory dir-path)]
                 #:when (file-exists? p))
        p)))
  ;; Filter by glob and skip hidden/VCS
  (define rx (glob->regexp glob-pattern #:allow-slash? #f))
  (for/list ([p (in-list all-paths)]
             #:when (let ([fname (path->string (file-name-from-path p))]) (regexp-match? rx fname))
             #:unless (should-skip-path? p))
    p))

;; ============================================================
;; Main tool function
;; ============================================================

(define (tool-grep args [exec-ctx #f])
  ;; (safe-mode path check is done by scheduler, not here)
  (define pattern (hash-ref args 'pattern #f))
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))

  (cond
    [(not pattern) (make-error-result "Missing required argument: pattern")]
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define glob-pattern (hash-ref args 'glob DEFAULT-GLOB))
     (define case-insensitive? (hash-ref args 'case-insensitive? DEFAULT-CASE-INSENSITIVE))
     (define max-results (hash-ref args 'max-results DEFAULT-MAX-RESULTS))
     (define context-lines (hash-ref args 'context-lines DEFAULT-CONTEXT-LINES))

     ;; 2. Compile pattern
     (define pattern-rx (compile-pattern pattern case-insensitive?))

     ;; 3. Resolve path
     (define the-path (string->path path-str))

     (cond
       [(not (or (file-exists? the-path) (directory-exists? the-path)))
        (make-error-result (format "Path not found: ~a" path-str))]
       [(file-exists? the-path)
        ;; Single file search
        (search-single-file the-path path-str pattern-rx context-lines max-results 1)]
       [(directory-exists? the-path)
        ;; Directory search
        (search-directory the-path path-str pattern-rx glob-pattern context-lines max-results)]
       [else (make-error-result (format "Path not found: ~a" path-str))])]))

;; ============================================================
;; Single file search
;; ============================================================

(define (search-single-file the-path path-str pattern-rx context-lines max-results files-searched)
  (define raw-bytes
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning (format "grep: ~a" (exn-message e)))
                                 #f)])
      (file->bytes the-path)))
  (cond
    [(not raw-bytes)
     ;; Unreadable file → treat as empty success
     (make-success-result
      '()
      (hasheq 'total-matches 0 'files-searched files-searched 'files-with-matches 0 'truncated? #f))]
    [(contains-null-bytes? raw-bytes)
     ;; Binary → skip silently, return empty success
     (make-success-result
      '()
      (hasheq 'total-matches 0 'files-searched files-searched 'files-with-matches 0 'truncated? #f))]
    [else
     (define text (bytes->string/utf-8 raw-bytes #\?))
     (define all-lines (string-split text "\n" #:trim? #f))
     (define has-trailing-newline
       (and (> (string-length text) 0)
            (char=? (string-ref text (sub1 (string-length text))) #\newline)))
     (define display-lines
       (if has-trailing-newline
           (drop-right all-lines 1)
           all-lines))
     (define lines-vec (list->vector display-lines))

     (define matches
       (for/list ([i (in-naturals 1)]
                  [line (in-list display-lines)]
                  #:when (regexp-match? pattern-rx line))
         (list i line)))

     (define total-matches (length matches))
     (define truncated? (> total-matches max-results))
     (define capped-matches (take matches (min total-matches max-results)))
     (define files-with-matches (if (> total-matches 0) 1 0))

     (define content-lines
       (apply append
              (for/list ([m (in-list capped-matches)])
                (format-match-with-context path-str (first m) (second m) lines-vec context-lines))))

     ;; Deduplicate and sort content lines (context may overlap)
     (define unique-lines (remove-duplicates content-lines string=?))

     (make-success-result unique-lines
                          (hasheq 'total-matches
                                  total-matches
                                  'files-searched
                                  files-searched
                                  'files-with-matches
                                  files-with-matches
                                  'truncated?
                                  truncated?))]))

;; ============================================================
;; Directory search
;; ============================================================

(define (search-directory the-path path-str pattern-rx glob-pattern context-lines max-results)
  (define files (collect-files path-str glob-pattern))

  (define all-results
    ;; (list file-path-str (list line-num line-text))
    (for/fold ([acc '()]
               #:result (reverse acc))
              ([f (in-list files)])
      (define f-str (path->string f))
      (define raw-bytes
        (with-handlers ([exn:fail? (lambda (e)
                                     (log-warning (format "grep: ~a" (exn-message e)))
                                     #f)])
          (file->bytes f)))
      (cond
        [(not raw-bytes) acc] ; unreadable → skip
        [(contains-null-bytes? raw-bytes) acc] ; binary → skip
        [else
         (define text (bytes->string/utf-8 raw-bytes #\?))
         (define all-lines (string-split text "\n" #:trim? #f))
         (define has-trailing-newline
           (and (> (string-length text) 0)
                (char=? (string-ref text (sub1 (string-length text))) #\newline)))
         (define display-lines
           (if has-trailing-newline
               (drop-right all-lines 1)
               all-lines))
         (define matches
           (for/list ([i (in-naturals 1)]
                      [line (in-list display-lines)]
                      #:when (regexp-match? pattern-rx line))
             (list i line)))
         (append acc (map (lambda (m) (list f-str m)) matches))])))

  ;; all-results : (listof (list file-str (list line-num line-text)))
  (define total-matches (length all-results))
  (define truncated? (> total-matches max-results))
  (define capped (take all-results (min total-matches max-results)))

  ;; Group by file to format with context
  ;; We need to load each file's lines for context
  (define content-lines
    (for/fold ([acc '()]) ([entry (in-list capped)])
      (define f-str (first entry))
      (define match-info (second entry))
      (define match-line-num (first match-info))
      (define raw-bytes
        (with-handlers ([exn:fail? (lambda (e)
                                     (log-warning (format "grep: ~a" (exn-message e)))
                                     #f)])
          (file->bytes f-str)))
      (if (not raw-bytes)
          acc
          (let* ([text (bytes->string/utf-8 raw-bytes #\?)]
                 [all-lines (string-split text "\n" #:trim? #f)]
                 [has-tnl (and (> (string-length text) 0)
                               (char=? (string-ref text (sub1 (string-length text))) #\newline))]
                 [display-lines (if has-tnl
                                    (drop-right all-lines 1)
                                    all-lines)]
                 [lines-vec (list->vector display-lines)]
                 [ctx (format-match-with-context f-str
                                                 match-line-num
                                                 (second match-info)
                                                 lines-vec
                                                 context-lines)])
            (append acc ctx)))))

  (define unique-lines (remove-duplicates content-lines string=?))

  (define files-with-matches (length (remove-duplicates (map first all-results) string=?)))

  (make-success-result unique-lines
                       (hasheq 'total-matches
                               total-matches
                               'files-searched
                               (length files)
                               'files-with-matches
                               files-with-matches
                               'truncated?
                               truncated?)))
