#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; Tests for metadata-backed suite classification in run-tests.rkt (v0.83.4 W0)

(require rackunit
         racket/file
         (only-in "../scripts/run-tests.rkt"
                  get-file-metadata
                  clear-metadata-cache!
                  slow-file?
                  tui-file?
                  mutating-file?))

;; Create temp test files with metadata and verify classification

(define (make-temp-test-file content)
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-foo.rkt"))
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'truncate)
  (values f dir))

(define (cleanup-temp-test-file dir)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files dir)))

;; ---------------------------------------------------------------------------
;; get-file-metadata
;; ---------------------------------------------------------------------------

(test-case "get-file-metadata: parses @speed fast"
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @speed fast\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'speed #f) 'fast)
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: parses @speed slow"
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @speed slow\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'speed #f) 'slow)
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: parses @suite tui"
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @suite tui\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'suite #f) "tui")
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: parses @mutates none"
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @mutates none\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'mutates #f) "none")
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: returns empty hash for no metadata"
  (define-values (f dir) (make-temp-test-file "#lang racket\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-false (hash-ref meta 'speed #f))
  (check-false (hash-ref meta 'suite #f))
  (check-false (hash-ref meta 'mutates #f))
  (cleanup-temp-test-file dir))

;; ---------------------------------------------------------------------------
;; slow-file? with metadata override
;; ---------------------------------------------------------------------------

(test-case "slow-file?: @speed fast overrides heuristic"
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-sandbox-foo.rkt"))
  (call-with-output-file f (lambda (out) (display "#lang racket\n;; @speed fast\n" out)))
  (check-false (slow-file? (path->string f)))
  (cleanup-temp-test-file dir))

(test-case "slow-file?: @speed slow forces slow classification"
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-quick-thing.rkt"))
  (call-with-output-file f (lambda (out) (display "#lang racket\n;; @speed slow\n" out)))
  (check-true (slow-file? (path->string f)))
  (cleanup-temp-test-file dir))

;; ---------------------------------------------------------------------------
;; tui-file? with metadata override
;; ---------------------------------------------------------------------------

(test-case "tui-file?: @suite tui forces tui classification"
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-something.rkt"))
  (call-with-output-file f (lambda (out) (display "#lang racket\n;; @suite tui\n" out)))
  (check-true (tui-file? (path->string f)))
  (cleanup-temp-test-file dir))

;; ---------------------------------------------------------------------------
;; mutating-file? with metadata override
;; ---------------------------------------------------------------------------

(test-case "mutating-file?: @mutates none overrides heuristic"
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-metrics-readme.rkt"))
  (call-with-output-file f (lambda (out) (display "#lang racket\n;; @mutates none\n" out)))
  (check-false (mutating-file? (path->string f)))
  (cleanup-temp-test-file dir))

;; ---------------------------------------------------------------------------
;; New metadata tags: boundary, isolation, timeout (F1)
;; ---------------------------------------------------------------------------

(test-case "get-file-metadata: parses @boundary integration"
  (clear-metadata-cache!)
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @boundary integration\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'boundary #f) "integration")
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: parses @isolation process"
  (clear-metadata-cache!)
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @isolation process\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'isolation #f) "process")
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: parses @timeout 30"
  (clear-metadata-cache!)
  (define-values (f dir) (make-temp-test-file "#lang racket\n;; @timeout 30\n"))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'timeout #f) 30)
  (cleanup-temp-test-file dir))

(test-case "mutating-file?: @isolation process forces mutating"
  (clear-metadata-cache!)
  (define dir (make-temporary-file "q-meta-test-~a" 'directory))
  (define f (build-path dir "test-clean-thing.rkt"))
  (call-with-output-file f (lambda (out) (display "#lang racket\n;; @isolation process\n" out)))
  (check-true (mutating-file? (path->string f)))
  (cleanup-temp-test-file dir))

(test-case "get-file-metadata: all 6 tags together"
  (clear-metadata-cache!)
  (define-values (f dir)
    (make-temp-test-file (string-append "#lang racket\n"
                                        ";; @speed slow\n"
                                        ";; @suite tui\n"
                                        ";; @mutates env\n"
                                        ";; @boundary integration\n"
                                        ";; @isolation process\n"
                                        ";; @timeout 45\n")))
  (define meta (get-file-metadata (path->string f)))
  (check-equal? (hash-ref meta 'speed #f) 'slow)
  (check-equal? (hash-ref meta 'suite #f) "tui")
  (check-equal? (hash-ref meta 'mutates #f) "env")
  (check-equal? (hash-ref meta 'boundary #f) "integration")
  (check-equal? (hash-ref meta 'isolation #f) "process")
  (check-equal? (hash-ref meta 'timeout #f) 45)
  (cleanup-temp-test-file dir))
