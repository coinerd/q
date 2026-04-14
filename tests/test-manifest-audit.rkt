#lang racket

;; tests/test-manifest-audit.rkt — Tests for extensions/manifest-audit.rkt
;;
;; Tests verify-package-checksum and audit-package functions.

(require rackunit
         racket/file
         racket/port
         json
         "../extensions/manifest-audit.rkt"
         "../extensions/manifest.rkt")

;; ============================================================
;; Helpers — create temporary package directories for testing
;; ============================================================

(define (make-tmp-pkg-dir name)
  (define dir (make-temporary-file (format "q-test-~a-~~a" name)))
  (delete-file dir)
  (make-directory* dir)
  dir)

(define (cleanup-pkg-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (write-qpm-json dir
                        #:name [name "test-pkg"]
                        #:version [version "0.1.0"]
                        #:api-version [api-version "1"]
                        #:type [type 'extension]
                        #:description [description "A test package"]
                        #:author [author "test"]
                        #:files [files '()]
                        #:checksum [checksum #f])
  (define m (make-qpm-manifest #:name name
                               #:version version
                               #:api-version api-version
                               #:type type
                               #:description description
                               #:author author
                               #:files files
                               #:checksum checksum))
  (write-qpm-manifest m (build-path dir "qpm.json")))

(define (touch-file dir rel-path)
  (define full-path (build-path dir rel-path))
  (make-directory* (path-only full-path))
  (call-with-output-file full-path void #:exists 'replace))

;; ============================================================
;; verify-package-checksum
;; ============================================================

(test-case "verify-package-checksum returns #f for directory without qpm.json"
  (define dir (make-tmp-pkg-dir "no-qpm"))
  (check-false (verify-package-checksum dir))
  (cleanup-pkg-dir dir))

(test-case "verify-package-checksum returns #t when manifest has no checksum"
  (define dir (make-tmp-pkg-dir "no-checksum"))
  (write-qpm-json dir)
  (check-true (verify-package-checksum dir))
  (cleanup-pkg-dir dir))

(test-case "verify-package-checksum returns #t with matching checksum"
  (define dir (make-tmp-pkg-dir "good-checksum"))
  (write-qpm-json dir #:files '("hello.rkt"))
  (touch-file dir "hello.rkt")
  ;; Compute the actual checksum and write it back
  (define computed (compute-manifest-checksum dir))
  (write-qpm-json dir #:files '("hello.rkt") #:checksum computed)
  (check-true (verify-package-checksum dir))
  (cleanup-pkg-dir dir))

(test-case "verify-package-checksum returns #f with wrong checksum"
  (define dir (make-tmp-pkg-dir "bad-checksum"))
  (write-qpm-json dir #:files '("hello.rkt") #:checksum "bogus-hash-value")
  (touch-file dir "hello.rkt")
  (check-false (verify-package-checksum dir))
  (cleanup-pkg-dir dir))

;; ============================================================
;; audit-package
;; ============================================================

(test-case "audit-package reports unreadable manifest"
  (define dir (make-tmp-pkg-dir "no-audit"))
  ;; No qpm.json written
  (define issues (audit-package dir))
  (check-true (pair? issues))
  (check-true (ormap (lambda (s) (string-contains? s "cannot read manifest")) issues))
  (cleanup-pkg-dir dir))

(test-case "audit-package reports missing files"
  (define dir (make-tmp-pkg-dir "missing-files"))
  (write-qpm-json dir #:files '("main.rkt" "helpers.rkt"))
  ;; Only create main.rkt, not helpers.rkt
  (touch-file dir "main.rkt")
  (define issues (audit-package dir))
  (check-true (ormap (lambda (s) (string-contains? s "missing file: helpers.rkt")) issues))
  (cleanup-pkg-dir dir))

(test-case "audit-package reports extra files"
  (define dir (make-tmp-pkg-dir "extra-files"))
  (write-qpm-json dir #:files '("main.rkt"))
  (touch-file dir "main.rkt")
  (touch-file dir "rogue-file.rkt")
  (define issues (audit-package dir))
  (check-true (ormap (lambda (s) (string-contains? s "extra file: rogue-file.rkt")) issues))
  (cleanup-pkg-dir dir))

(test-case "audit-package reports checksum mismatch"
  (define dir (make-tmp-pkg-dir "audit-bad-checksum"))
  (write-qpm-json dir #:files '("data.rkt") #:checksum "wrong-checksum")
  (touch-file dir "data.rkt")
  (define issues (audit-package dir))
  (check-true (ormap (lambda (s) (string-contains? s "checksum mismatch")) issues))
  (cleanup-pkg-dir dir))

(test-case "audit-package returns empty list for clean package"
  (define dir (make-tmp-pkg-dir "clean-pkg"))
  (write-qpm-json dir #:files '("main.rkt"))
  (touch-file dir "main.rkt")
  (define issues (audit-package dir))
  (check-equal? issues '())
  (cleanup-pkg-dir dir))

(test-case "audit-package ignores compiled/ directory and qpm.json"
  (define dir (make-tmp-pkg-dir "ignore-dirs"))
  (write-qpm-json dir #:files '("main.rkt"))
  (touch-file dir "main.rkt")
  ;; These should not be flagged as extra
  (make-directory* (build-path dir "compiled"))
  (touch-file dir "compiled/main_rkt.zo")
  (define issues (audit-package dir))
  (check-false (ormap (lambda (s) (string-contains? s "extra file")) issues))
  (cleanup-pkg-dir dir))
