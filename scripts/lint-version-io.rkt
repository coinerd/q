#lang racket/base

;; scripts/lint-version-io.rkt — I/O abstraction for version linting
;;
;; W6 (#8419): Port/I/O abstraction pilot for report/version tooling.
;;
;; Makes file-reading operations injectable via parameters, enabling tests to
;; substitute in-memory mock file systems without touching the real filesystem.
;;
;; Pattern:
;;   (parameterize ([current-lint-file->string (make-mock-read-string fs)]
;;                  [current-lint-file-exists? (make-mock-exists fs)])
;;     ... check logic ...)
;;
;; Default behavior: all parameters use real file I/O (racket/file).
;;
;; A "mock file system" is a hash: path-string -> content-string.
;; Use `make-mock-fs` to create one from an alist of (path . content).

(require racket/file
         racket/string)

;; Parameters
(provide current-lint-file-exists?
         current-lint-file->string
         current-lint-file->lines
         current-lint-read-md-paths
         ;; Mock file system
         make-mock-fs
         make-mock-exists
         make-mock-read-string
         make-mock-read-lines
         make-mock-md-paths)

;; ---------------------------------------------------------------------------
;; Parameters (default: real file I/O)
;; ---------------------------------------------------------------------------

(define current-lint-file-exists? (make-parameter file-exists?))
(define current-lint-file->string (make-parameter file->string))
(define current-lint-file->lines (make-parameter file->lines))
;; Returns a function: (base-path -> (listof path-string))
(define current-lint-read-md-paths (make-parameter #f))

;; ---------------------------------------------------------------------------
;; Mock file system
;; ---------------------------------------------------------------------------

;; Create a mock file system hash from an alist: (path-string . content-string)
(define (make-mock-fs alist)
  (for/hash ([pair (in-list alist)])
    (values (car pair) (cdr pair))))

;; Create a mock file-exists? function from a mock fs.
;; Accepts path or string; normalizes to string for lookup.
(define (make-mock-exists fs)
  (λ (path) (hash-has-key? fs (path->key path))))

;; Create a mock file->string function from a mock fs.
(define (make-mock-read-string fs)
  (λ (path)
    (define key (path->key path))
    (unless (hash-has-key? fs key)
      (error 'file->string "no such file: ~a" key))
    (hash-ref fs key)))

;; Create a mock file->lines function from a mock fs.
(define (make-mock-read-lines fs)
  (λ (path)
    (define key (path->key path))
    (unless (hash-has-key? fs key)
      (error 'file->lines "no such file: ~a" key))
    (string-split (hash-ref fs key) "\n")))

;; Create a mock read-md-paths function that returns a fixed list.
;; Returns a function: (base-path -> (listof path-string))
(define (make-mock-md-paths paths)
  (λ base-dir paths))

;; ---------------------------------------------------------------------------
;; Path normalization helper
;; ---------------------------------------------------------------------------

(define (path->key p)
  (if (path? p)
      (path->string p)
      p))
