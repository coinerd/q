#lang racket/base

(require racket/contract
         racket/file
         racket/match
         racket/string
         (only-in racket/list last drop)
         (only-in "../tool.rkt" make-success-result make-error-result exec-context? tool-result?)
         (only-in "../exec-context.rkt" exec-context-working-directory)
         (only-in "../../extensions/gsd/session-state.rkt"
                  [current-edit-limit current-max-old-text-len]
                  [set-edit-limit! set-current-max-old-text-len!])
         (only-in "../../util/path/path-helpers.rkt" expand-home-path)
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         (only-in "../../util/racket-source-validation.rkt"
                  validate-proposed-racket-source
                  racket-edit-balance-warning)
         (only-in "builtin-helpers.rkt" require-safe-path! check-utf8-file? validate-utf8-bytes)
         "edit-contract.rkt"
         "atomic-file-replace.rkt"
         "../../util/config-paths.rkt")

(define current-fuzzy-edit-enabled? (make-parameter #f))
(define current-edit-before-replace-hook (make-parameter void))
(define current-edit-before-final-replace-hook (make-parameter void))

(provide current-max-old-text-len
         set-current-max-old-text-len!
         current-fuzzy-edit-enabled?
         current-edit-before-replace-hook
         current-edit-before-final-replace-hook
         SAFE-MAX-OLD-TEXT-LEN
         (contract-out [tool-edit (->* (hash?) ((or/c exec-context? #f)) tool-result?)]))

;; --------------------------------------------------
;; Backup helpers
;; --------------------------------------------------

(define MAX-BACKUPS-PER-FILE 10)

(define (ensure-backup-dir)
  (define dir (build-path (global-config-dir) "edit-backups"))
  (unless (directory-exists? dir)
    (make-directory* dir)
    (file-or-directory-permissions dir #o700))
  dir)

(define (save-backup path-str content)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/backup: ~a" (exn-message e)))
                               #f)])
    (define dir (ensure-backup-dir))
    (define basename (file-name-from-path path-str))
    (define source-key (number->string (equal-hash-code path-str) 16))
    (define timestamp (number->string (abs (current-milliseconds))))
    ;; Exclusive creation prevents concurrent edits from overwriting backups.
    (define backup-path
      (make-temporary-file (format "~a_~a_~a_~a" timestamp source-key "~a" basename) #:base-dir dir))
    (display-to-file content backup-path #:exists 'truncate)
    (prune-old-backups dir source-key basename)
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

(define (prune-old-backups dir source-key basename)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/prune: ~a" (exn-message e)))
                               (void))])
    (define marker (format "_~a_" source-key))
    (define all (directory-list dir))
    (define matching
      (filter (lambda (f)
                (define name (path->string f))
                (and (string-contains? name marker) (string-suffix? name (format "_~a" basename))))
              (sort (map path->string all) string>?)))
    (when (> (length matching) MAX-BACKUPS-PER-FILE)
      (for ([f (in-list (drop matching MAX-BACKUPS-PER-FILE))])
        (delete-file (build-path dir f))))))

;; --------------------------------------------------
;; Enhanced not-found diagnostics
;; --------------------------------------------------

(define (first-differing-offset a b)
  (define len (min (string-length a) (string-length b)))
  (let loop ([i 0])
    (cond
      [(>= i len)
       (if (= (string-length a) (string-length b))
           (values #f #f #f)
           (values len #f #f))]
      [(char=? (string-ref a i) (string-ref b i)) (loop (add1 i))]
      [else (values i (string-ref a i) (string-ref b i))])))

(define (escape-char c)
  (format "U+~X" (char->integer c)))

(define (escaped-context s offset [context-radius 6])
  (define start (max 0 (- offset context-radius)))
  (define end (min (string-length s) (+ offset context-radius)))
  (define parts
    (for/list ([i (in-range start end)])
      (escape-char (string-ref s i))))
  (format "[~a]" (string-join parts " ")))

(define (count-leading-spaces s)
  (for/fold ([count 0])
            ([ch (in-string s)]
             #:break (not (char=? ch #\space)))
    (add1 count)))

(define (make-not-found-error path-str old-text content)
  (define-values (line-num line-text) (find-nearest-match content old-text))
  (define-values (diff-offset content-char old-char) (first-differing-offset content old-text))
  (define diff-detail
    (cond
      [(and diff-offset content-char old-char)
       (format
        "First differing offset: ~a (~a vs ~a)\nContext around mismatch in file:  ~a\nContext around mismatch in old-text: ~a"
        diff-offset
        (escape-char content-char)
        (escape-char old-char)
        (escaped-context content diff-offset)
        (escaped-context old-text diff-offset))]
      [diff-offset
       (format "First differing offset: ~a (file has ~a chars, old-text has ~a chars)\n"
               diff-offset
               (string-length content)
               (string-length old-text))]
      [else ""]))
  (define ws-info
    (let* ([c-lines (string-split content "\n" #:trim? #f)]
           [o-lines (string-split old-text "\n" #:trim? #f)]
           [diff-lines (for/list ([c (in-list c-lines)]
                                  [o (in-list o-lines)]
                                  #:when (and c o (not (equal? c o))))
                         (define c-spaces (count-leading-spaces c))
                         (define o-spaces (count-leading-spaces o))
                         (format "  file has ~a leading spaces, old-text has ~a" c-spaces o-spaces))])
      (if (pair? diff-lines)
          (string-append "Whitespace differences:\n" (string-join diff-lines "\n"))
          "")))
  (define hint
    (match old-text
      [(? (lambda (s) (and (string? s) (regexp-match? #rx"^ +" s))))
       "Hint: old-text has leading whitespace -- check indentation."]
      [(? (lambda (s)
            (and (string? s) (> (string-length s) 200) (< (length (string-split s "\n")) 2))))
       "Hint: old-text is very long and single-line -- try a smaller unique snippet."]
      [_ ""]))
  (define base
    (match line-num
      [#f
       (format "old-text not found in ~a (appears 0 times). Read the file first to get exact text.\n"
               path-str)]
      [_
       (string-append
        (format "old-text not found in ~a.\nNearest match at line ~a:\n  \"" path-str line-num)
        (string-trim line-text)
        "\"\n")]))
  (string-append base
                 (if (equal? diff-detail "")
                     ""
                     (string-append diff-detail "\n"))
                 ws-info
                 (if (and (not (equal? ws-info "")) (not (equal? hint ""))) "\n" "")
                 hint))

;; --------------------------------------------------
;; Near-match helper
;; --------------------------------------------------

(define (longest-common-substring-len a b)
  (define la (string-length a))
  (define lb (string-length b))
  (cond
    [(or (zero? la) (zero? lb)) 0]
    [else
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

(define (extract-search-key old-text)
  (define trimmed (string-trim old-text))
  (if (<= (string-length trimmed) 60)
      trimmed
      (substring trimmed 0 60)))

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

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define expanded (and raw-path (expand-home-path raw-path)))
  (define wd (and exec-ctx (exec-context-working-directory exec-ctx)))
  (define path-str
    (and expanded
         (parameterize ([current-directory (or wd (current-directory))])
           (let ([p (if (string? expanded)
                        expanded
                        (path->string expanded))])
             (with-handlers ([exn:fail? (lambda (e)
                                          (log-warning "edit: canonicalize failed: ~a"
                                                       (exn-message e))
                                          p)])
               (path->string (simplify-path (resolve-path (path->complete-path p)))))))))
  ;; W4: Path identity hardening
  (define identity-ok?
    (and path-str
         expanded
         (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
           (let ([direct-id (file-or-directory-identity path-str)])
             (define re-resolved
               (parameterize ([current-directory (or wd (current-directory))])
                 (path->string (simplify-path (resolve-path (path->complete-path expanded))))))
             (and (file-exists? re-resolved)
                  (= direct-id (file-or-directory-identity re-resolved)))))))
  (match (list path-str (hash-ref args 'old-text #f) (hash-ref args 'new-text #f))
    [(list #f _ _) (make-error-result "Missing required argument: path")]
    [(list _ #f _) (make-error-result "Missing required argument: old-text")]
    [(list _ _ #f) (make-error-result "Missing required argument: new-text")]
    [(list (? string? path) (? string? old-text) (? string? new-text))
     (cond
       [(and (hash-has-key? args 'fuzzy?) (not (boolean? (hash-ref args 'fuzzy?))))
        (make-error-result "fuzzy? must be a boolean")]
       [(not (boolean? (current-fuzzy-edit-enabled?)))
        (make-error-result "Invalid fuzzy edit policy")]
       [(require-safe-path! path "edit")
        =>
        (lambda (err) (make-error-result err))]
       [(let ([provided (hash-ref args 'max-old-text-len #f)])
          (and provided
               (or (not (exact-positive-integer? provided)) (> provided SAFE-MAX-OLD-TEXT-LEN))))
        (make-error-result (format "max-old-text-len must be an exact positive integer at most ~a"
                                   SAFE-MAX-OLD-TEXT-LEN))]
       [(not (file-exists? path)) (make-error-result (format "File not found: ~a" path))]
       [(not identity-ok?)
        (make-error-result (format "Path identity mismatch for ~a: possible symlink swap" path))]
       [else
        (define initial-bytes (file->bytes path))
        (define utf8-check (validate-utf8-bytes initial-bytes))
        (if (string? utf8-check)
            (make-error-result (format "File ~a is not valid UTF-8: ~a" path utf8-check))
            (let ()
              (define initial-identity (file-or-directory-identity path))
              (define permission-bits (file-or-directory-permissions path 'bits))
              (define initial-modify-seconds (file-or-directory-modify-seconds path))
              (define initial-size (file-size path))
              (define content (bytes->string/utf-8 initial-bytes))
              (define provided-limit (hash-ref args 'max-old-text-len #f))
              (define max-old-text-len
                (if provided-limit
                    provided-limit
                    (current-max-old-text-len)))
              (define global-fuzzy-enabled? (current-fuzzy-edit-enabled?))
              (define fuzzy-allowed? (or (hash-ref args 'fuzzy? #f) global-fuzzy-enabled?))
              (define edit-result
                (apply-edit-contract content
                                     old-text
                                     new-text
                                     #:fuzzy? fuzzy-allowed?
                                     #:max-old-text-len max-old-text-len))
              (match (edit-contract-result-status edit-result)
                ['empty-old-text
                 (make-error-result "old-text must not be empty; provide one unique exact snippet")]
                ['too-long
                 (make-error-result
                  (format
                   (string-append "old-text is too long (~a chars, max ~a). "
                                  "For a whole-form replacement, pass max-old-text-len explicitly "
                                  "(up to ~a), or use the structural edit tool; "
                                  "do not split a nested form into partial edits.")
                   (string-length old-text)
                   max-old-text-len
                   SAFE-MAX-OLD-TEXT-LEN))]
                ['not-found (make-error-result (make-not-found-error path old-text content))]
                ['duplicate
                 (make-error-result
                  (format "old-text appears ~a times in ~a; provide one unique exact snippet"
                          (edit-contract-result-occurrences edit-result)
                          path))]
                ['ambiguous
                 (make-error-result
                  (format
                   "Fuzzy matching found ~a possible matches in ~a; re-read and provide exact text"
                   (edit-contract-result-occurrences edit-result)
                   path))]
                ['line-count-mismatch
                 (make-error-result
                  (string-append "Edit rejected: line count changed unexpectedly. "
                                 "The file may have been modified since your last read. "
                                 "Re-read the file and try a smaller edit."))]
                ['ok
                 (define new-content (edit-contract-result-content edit-result))
                 (define balance-warning (racket-edit-balance-warning path old-text new-text))
                 (define parse-error (validate-proposed-racket-source path new-content))
                 (if parse-error
                     (make-error-result (if balance-warning
                                            (string-append parse-error "\n" balance-warning)
                                            parse-error))
                     (let ([backup-path (save-backup path content)])
                       (with-handlers ([exn:fail:filesystem?
                                        (lambda (e)
                                          (make-error-result (sanitize-error-message
                                                              (format "Write error: ~a"
                                                                      (exn-message e)))))])
                         (define replaced?
                           (atomic-replace-file-if-unchanged
                            path
                            initial-identity
                            initial-bytes
                            permission-bits
                            initial-modify-seconds
                            initial-size
                            new-content
                            #:before-guard (current-edit-before-replace-hook)
                            #:before-final-guard (current-edit-before-final-replace-hook)))
                         (if replaced?
                             (make-success-result
                              (list
                               (let ([base (format "Edited ~a (replaced ~a occurrence)"
                                                   path
                                                   (edit-contract-result-replacements edit-result))])
                                 (if balance-warning
                                     (string-append base "\n" balance-warning)
                                     base)))
                              (hasheq 'path
                                      path
                                      'replacements
                                      (edit-contract-result-replacements edit-result)
                                      'old-length
                                      (string-length old-text)
                                      'new-length
                                      (string-length new-text)
                                      'backup
                                      (or backup-path "")))
                             (make-error-result
                              (string-append
                               "Edit aborted: the file changed since it was read. "
                               "The newer file was left untouched, re-read and retry."))))))])))])]))
