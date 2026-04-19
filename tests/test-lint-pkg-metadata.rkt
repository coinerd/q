#lang racket

;; test-lint-pkg-metadata.rkt — Tests for scripts/lint-pkg-metadata.rkt
;;
;; FIX-09: Dedicated tests for lint-info-rkt and lint-index functions

(require rackunit
         racket/file
         racket/port
         racket/string
         json
         (only-in "../scripts/lint-pkg-metadata.rkt" lint-info-rkt lint-index))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-tmp-dir)
  (define tmp (make-temporary-file "lint-pkg-test-~a"))
  (delete-file tmp)
  (make-directory tmp)
  tmp)

(define (cleanup tmp)
  (when (directory-exists? tmp)
    (delete-directory/files tmp)))

;; ---------------------------------------------------------------------------
;; Tests: lint-info-rkt
;; ---------------------------------------------------------------------------

(test-case "lint-info-rkt: valid info.rkt passes"
  (define tmp (make-tmp-dir))
  (define info-path (build-path tmp "info.rkt"))
  (call-with-output-file info-path
                         (λ (out) (display "#lang info\n(define collection \"q\")" out))
                         #:exists 'truncate)
  (define errors (lint-info-rkt (path->string info-path)))
  (check-equal? errors '())
  (cleanup tmp))

(test-case "lint-info-rkt: missing file returns error"
  (define errors (lint-info-rkt "/nonexistent/path/info.rkt"))
  (check-true (> (length errors) 0))
  (check-true (ormap (λ (e) (string-contains? e "not found")) errors)))

(test-case "lint-info-rkt: wrong #lang reported"
  (define tmp (make-tmp-dir))
  (define info-path (build-path tmp "info.rkt"))
  (call-with-output-file info-path (λ (out) (display "#lang racket/base" out)) #:exists 'truncate)
  (define errors (lint-info-rkt (path->string info-path)))
  (check-true (> (length errors) 0))
  (check-true (ormap (λ (e) (string-contains? e "#lang info")) errors))
  (cleanup tmp))

;; ---------------------------------------------------------------------------
;; Tests: lint-index
;; ---------------------------------------------------------------------------

(test-case "lint-index: missing index returns early with warning"
  (define errors (lint-index "/nonexistent/path/index.json"))
  ;; Should return '() — missing file is a warning, not an error
  (check-equal? errors '()))

(test-case "lint-index: valid index passes"
  (define tmp (make-tmp-dir))
  (define idx-path (build-path tmp "index.json"))
  (define idx
    (hasheq 'version
            1
            'updated
            "2026-04-19T00:00:00Z"
            'packages
            (list (hasheq 'name
                          "q-test-pkg"
                          'version
                          "1.0.0"
                          'description
                          "Test"
                          'author
                          "test"
                          'repo
                          "https://github.com/test/test"
                          'checksum
                          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                          'keywords
                          '("test")))))
  (call-with-output-file idx-path (λ (out) (write-json idx out)) #:exists 'truncate)
  (define errors (lint-index (path->string idx-path)))
  (check-equal? errors '())
  (cleanup tmp))
