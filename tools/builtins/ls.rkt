#lang racket/base

(require racket/file
         racket/format
         racket/list
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/path-filters.rkt" hidden-name?))

(provide tool-ls)

;; --------------------------------------------------
;; Entry classification
;; --------------------------------------------------

(define (entry-type base-path name)
  (define full (build-path base-path name))
  (cond
    [(link-exists? full) 'link]
    [(directory-exists? full) 'dir]
    [else 'file]))

(define (type-indicator t)
  (case t
    [(dir) #\d]
    [(link) #\l]
    [else #\-]))

(define (entry-size base-path name type)
  (if (eq? type 'file)
      (file-size (build-path base-path name))
      #f))

(define (entry-mtime base-path name)
  (file-or-directory-modify-seconds (build-path base-path name)))

;; --------------------------------------------------
;; Sorting
;; --------------------------------------------------

(define (sort-entries entries base-path sort-by)
  (case sort-by
    [("size")
     (sort entries > #:key (λ (e) (or (entry-size base-path (entry-name e) (entry-type-e e)) -1)))]
    [("date") (sort entries > #:key (λ (e) (entry-mtime base-path (entry-name e))))]
    [else ; "name"
     ;; Directories first, then alphabetical
     (define dirs (filter (λ (e) (memq (entry-type-e e) '(dir link))) entries))
     (define files (filter (λ (e) (eq? (entry-type-e e) 'file)) entries))
     (define (name<? a b)
       (string-ci<? (entry-name a) (entry-name b)))
     (append (sort dirs name<?) (sort files name<?))]))

;; Lightweight entry struct for sorting
(struct entry (name type-e) #:transparent)

;; --------------------------------------------------
;; Formatting
;; --------------------------------------------------

(define (format-short name)
  name)

(define (format-long name type size)
  (define size-str
    (if size
        (~a size)
        "-"))
  (format "~a ~a ~a" (type-indicator type) size-str name))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-ls args [exec-ctx #f])
  ;; (safe-mode path check is done by scheduler, not here)
  (define path-str (hash-ref args 'path #f))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]

    ;; 2. Path must exist
    [(not (directory-exists? path-str))
     (cond
       [(file-exists? path-str) (make-error-result (format "Not a directory: ~a" path-str))]
       [else (make-error-result (format "Path not found: ~a" path-str))])]

    [else
     ;; 3. Read directory entries
     (define show-hidden? (hash-ref args 'all? #f))
     (define long? (hash-ref args 'long? #f))
     (define sort-by (hash-ref args 'sort-by "name"))

     (define raw-names (directory-list path-str))
     (define filtered
       (if show-hidden?
           (map path->string raw-names)
           (filter (λ (n) (not (hidden-name? n))) (map path->string raw-names))))

     ;; 4. Build entry structs
     (define entries
       (for/list ([name (in-list filtered)])
         (entry name (entry-type path-str name))))

     ;; 5. Sort
     (define sorted (sort-entries entries path-str sort-by))

     ;; 6. Format output
     (define content-lines
       (for/list ([e (in-list sorted)])
         (define name (entry-name e))
         (define type (entry-type-e e))
         (if long?
             (format-long name type (entry-size path-str name type))
             (format-short name))))

     ;; 7. Count dirs and files
     (define dir-count (count (λ (e) (memq (entry-type-e e) '(dir link))) sorted))
     (define file-count (count (λ (e) (eq? (entry-type-e e) 'file)) sorted))

     (make-success-result content-lines
                          (hasheq 'total-entries
                                  (length sorted)
                                  'path
                                  path-str
                                  'directories
                                  dir-count
                                  'files
                                  file-count))]))
