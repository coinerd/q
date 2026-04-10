#lang racket

;; tests/test-checksum.rkt -- Tests for q/util/checksum.rkt

(require rackunit
         racket/file
         racket/port
         "../util/checksum.rkt"
         "../extensions/manifest.rkt"
         "../extensions/manifest-audit.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (sample-manifest #:name [name "q-test"]
                          #:version [version "1.0.0"]
                          #:api-version [api-version "1"]
                          #:type [type 'extension]
                          #:description [description "Test extension"]
                          #:author [author "test"]
                          #:files [files '()]
                          #:checksum [checksum #f])
  (make-qpm-manifest #:name name
                     #:version version
                     #:api-version api-version
                     #:type type
                     #:description description
                     #:author author
                     #:files files
                     #:checksum checksum))

(define (make-test-package dir
                            #:files [file-alist '()]
                            #:checksum [checksum #f])
  ;; file-alist: list of (relative-path . content-string)
  (define file-names (map car file-alist))
  ;; Write files
  (for ([entry (in-list file-alist)])
    (define fpath (build-path dir (car entry)))
    (define parent (path-only fpath))
    (when parent (make-directory* parent))
    (call-with-output-file fpath
      (lambda (out) (display (cdr entry) out))
      #:exists 'replace))
  ;; Write manifest
  (define m (sample-manifest #:files file-names #:checksum checksum))
  (write-qpm-manifest m (build-path dir "qpm.json"))
  m)

;; ============================================================
;; 1. sha256-string returns 64-char hex
;; ============================================================

(let ([h (sha256-string "hello")])
  (check-equal? (string-length h) 64)
  (check-not-false (regexp-match? #px"^[0-9a-f]{64}$" h)
              (format "expected 64 hex chars, got: ~a" h)))

;; ============================================================
;; 2. sha256-string is deterministic
;; ============================================================

(check-equal? (sha256-string "hello") (sha256-string "hello"))
(check-equal? (sha256-string "") (sha256-string ""))

;; ============================================================
;; 3. sha256-string different for different inputs
;; ============================================================

(check-not-equal? (sha256-string "hello") (sha256-string "world"))
(check-not-equal? (sha256-string "foo") (sha256-string "bar"))

;; ============================================================
;; 4. sha256-file matches sha256-string for same content
;; ============================================================

(let ([tmp (make-temporary-file "q-test-~a.txt")])
  (call-with-output-file tmp
    (lambda (out) (display "file content here" out))
    #:exists 'replace)
  (check-equal? (sha256-file tmp) (sha256-string "file content here"))
  (delete-file tmp))

;; ============================================================
;; 5. verify-file-checksum returns #t for match
;; ============================================================

(let* ([tmp (make-temporary-file "q-test-~a.txt")]
       [_ (call-with-output-file tmp
            (lambda (out) (display "test content" out))
            #:exists 'replace)]
       [h (sha256-file tmp)])
  (check-true (verify-file-checksum tmp h))
  (delete-file tmp))

;; ============================================================
;; 6. verify-file-checksum returns #f for mismatch
;; ============================================================

(let ([tmp (make-temporary-file "q-test-~a.txt")])
  (call-with-output-file tmp
    (lambda (out) (display "test content" out))
    #:exists 'replace)
  (check-false (verify-file-checksum tmp "0000000000000000000000000000000000000000000000000000000000000000"))
  (delete-file tmp))

;; ============================================================
;; 7. verify-package-checksum returns #t for valid package
;; ============================================================

(let ([dir (make-temporary-file "q-test-~a" 'directory)])
  ;; No checksum field → should return #t
  (make-test-package dir
    #:files '(("main.rkt" . "#lang racket/base\n(+ 1 2)"))
    #:checksum #f)
  (check-true (verify-package-checksum dir))
  ;; Now compute and set checksum
  (let ([cs (compute-manifest-checksum dir)])
    (make-test-package dir
      #:files '(("main.rkt" . "#lang racket/base\n(+ 1 2)"))
      #:checksum cs)
    (check-true (verify-package-checksum dir)))
  (delete-directory/files dir))

;; ============================================================
;; 8. verify-package-checksum returns #f for tampered package
;; ============================================================

(let ([dir (make-temporary-file "q-test-~a" 'directory)])
  ;; Write original and compute checksum
  (make-test-package dir
    #:files '(("main.rkt" . "original content"))
    #:checksum #f)
  (let ([cs (compute-manifest-checksum dir)])
    ;; Tamper with file, but keep old checksum
    (call-with-output-file (build-path dir "main.rkt")
      (lambda (out) (display "tampered content" out))
      #:exists 'replace)
    (let ([m (sample-manifest #:files '("main.rkt") #:checksum cs)])
      (write-qpm-manifest m (build-path dir "qpm.json")))
    (check-false (verify-package-checksum dir)))
  (delete-directory/files dir))

;; ============================================================
;; 9. audit-package returns empty list for clean package
;; ============================================================

(let ([dir (make-temporary-file "q-test-~a" 'directory)])
  (make-test-package dir
    #:files '(("main.rkt" . "(+ 1 2)") ("lib.rkt" . "(+ 3 4)")))
  (check-equal? (audit-package dir) '())
  (delete-directory/files dir))

;; ============================================================
;; 10. audit-package reports missing files
;; ============================================================

(let ([dir (make-temporary-file "q-test-~a" 'directory)])
  ;; Create manifest listing files that don't exist
  (let ([m (sample-manifest #:files '("main.rkt" "missing.rkt"))])
    (write-qpm-manifest m (build-path dir "qpm.json")))
  ;; Only create main.rkt
  (call-with-output-file (build-path dir "main.rkt")
    (lambda (out) (display "exists" out))
    #:exists 'replace)
  (let ([issues (audit-package dir)])
    (check-not-false (member "missing file: missing.rkt" issues)
                (format "expected missing file issue, got: ~a" issues)))
  (delete-directory/files dir))

;; ============================================================
;; 11. audit-package reports extra files
;; ============================================================

(let ([dir (make-temporary-file "q-test-~a" 'directory)])
  (make-test-package dir
    #:files '(("main.rkt" . "(+ 1 2)"))
    #:checksum #f)
  ;; Add an untracked file
  (call-with-output-file (build-path dir "rogue.rkt")
    (lambda (out) (display "rogue" out))
    #:exists 'replace)
  (let ([issues (audit-package dir)])
    (check-not-false (member "extra file: rogue.rkt" issues)
                (format "expected extra file issue, got: ~a" issues)))
  (delete-directory/files dir))
