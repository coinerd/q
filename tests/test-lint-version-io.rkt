#lang racket

;; @speed fast
;; @suite default

;; test-lint-version-io.rkt — Tests for I/O abstraction (W6 pilot)
;;
;; Tests the parameterized I/O interface for version linting.

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

(define-runtime-path io-path "../scripts/lint-version-io.rkt")

(define (io-ref sym)
  (dynamic-require io-path sym))

;; ---------------------------------------------------------------------------
;; Parameter defaults
;; ---------------------------------------------------------------------------

(define-test-suite lint-version-io-tests
                   (test-case "current-lint-file-exists? is a parameter"
                     (define p (io-ref 'current-lint-file-exists?))
                     (check-true (procedure? p)))
                   (test-case "current-lint-file->string is a parameter"
                     (define p (io-ref 'current-lint-file->string))
                     (check-true (procedure? p)))
                   (test-case "current-lint-file->lines is a parameter"
                     (define p (io-ref 'current-lint-file->lines))
                     (check-true (procedure? p)))
                   (test-case "current-lint-read-md-paths is a parameter"
                     (define p (io-ref 'current-lint-read-md-paths))
                     (check-true (procedure? p)))
                   ;; ---------------------------------------------------------------------------
                   ;; Mock file system
                   ;; ---------------------------------------------------------------------------
                   (test-case "make-mock-fs creates a hash from alist"
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define fs (make-mock-fs '(("info.rkt" . "(define version \"1.0.0\")"))))
                     (check-true (hash? fs))
                     (check-equal? (hash-ref fs "info.rkt") "(define version \"1.0.0\")"))
                   ;; ---------------------------------------------------------------------------
                   ;; File exists check with mock
                   ;; ---------------------------------------------------------------------------
                   (test-case "mock file-exists? returns true for known files"
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define make-mock-exists (io-ref 'make-mock-exists))
                     (define fs (make-mock-fs '(("info.rkt" . "content"))))
                     (define exists? (make-mock-exists fs))
                     (check-true (exists? "info.rkt"))
                     (check-false (exists? "nonexistent.rkt")))
                   ;; ---------------------------------------------------------------------------
                   ;; File->string with mock
                   ;; ---------------------------------------------------------------------------
                   (test-case "mock file->string returns content for known files"
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define make-mock-read (io-ref 'make-mock-read-string))
                     (define fs (make-mock-fs '(("README.md" . "# Hello"))))
                     (define reader (make-mock-read fs))
                     (check-equal? (reader "README.md") "# Hello"))
                   (test-case "mock file->string errors on unknown files"
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define make-mock-read (io-ref 'make-mock-read-string))
                     (define fs (make-mock-fs '()))
                     (define reader (make-mock-read fs))
                     (check-exn exn:fail? (λ () (reader "missing.md"))))
                   ;; ---------------------------------------------------------------------------
                   ;; Parameterize with mock I/O
                   ;; ---------------------------------------------------------------------------
                   (test-case "parameterize overrides default I/O"
                     (define param (io-ref 'current-lint-file->string))
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define make-mock-read (io-ref 'make-mock-read-string))
                     (define fs (make-mock-fs '(("test.rkt" . "hello"))))
                     (define reader (make-mock-read fs))
                     (parameterize ([param reader])
                       (define current-read (io-ref 'current-lint-file->string))
                       (check-equal? ((current-read) "test.rkt") "hello")))
                   ;; ---------------------------------------------------------------------------
                   ;; Mock file->lines
                   ;; ---------------------------------------------------------------------------
                   (test-case "mock file->lines splits content by newlines"
                     (define make-mock-fs (io-ref 'make-mock-fs))
                     (define make-mock-read-lines (io-ref 'make-mock-read-lines))
                     (define fs (make-mock-fs '(("test.md" . "line1\nline2\nline3"))))
                     (define reader (make-mock-read-lines fs))
                     (check-equal? (reader "test.md") '("line1" "line2" "line3")))
                   ;; ---------------------------------------------------------------------------
                   ;; Mock read-md-paths
                   ;; ---------------------------------------------------------------------------
                   (test-case "make-mock-md-paths returns configured list"
                     (define make-mock-md-paths (io-ref 'make-mock-md-paths))
                     (define paths-proc (make-mock-md-paths '("README.md" "docs/guide.md")))
                     (check-equal? (paths-proc) '("README.md" "docs/guide.md"))))

(run-tests lint-version-io-tests)
