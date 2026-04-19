#lang racket

(require rackunit
         "../extensions/manifest.rkt"
         racket/file
         racket/port)

;; ============================================================
;; Helper: create a valid manifest
;; ============================================================

(define (sample-manifest #:name [name "q-todo"]
                         #:version [version "1.0.0"]
                         #:api-version [api-version "1"]
                         #:type [type 'extension]
                         #:description [description "A todo extension"]
                         #:author [author "coinerd"]
                         #:compat [compat #f]
                         #:files [files '()]
                         #:checksum [checksum #f]
                         #:entry [entry #f]
                         #:homepage [homepage #f]
                         #:license [license #f])
  (make-qpm-manifest #:name name
                     #:version version
                     #:api-version api-version
                     #:type type
                     #:description description
                     #:author author
                     #:compat compat
                     #:files files
                     #:checksum checksum
                     #:entry entry
                     #:homepage homepage
                     #:license license))

;; ============================================================
;; 1. Valid manifest creation and field access
;; ============================================================

(test-case "valid manifest creation and field access"
  (define m (sample-manifest))
  (check-equal? (qpm-manifest-name m) "q-todo")
  (check-equal? (qpm-manifest-version m) "1.0.0")
  (check-equal? (qpm-manifest-api-version m) "1")
  (check-equal? (qpm-manifest-type m) 'extension)
  (check-equal? (qpm-manifest-description m) "A todo extension")
  (check-equal? (qpm-manifest-author m) "coinerd")
  (check-equal? (qpm-manifest-compat m) #f)
  (check-equal? (qpm-manifest-files m) '())
  (check-equal? (qpm-manifest-checksum m) #f)
  (check-equal? (qpm-manifest-entry m) #f)
  (check-equal? (qpm-manifest-homepage m) #f)
  (check-equal? (qpm-manifest-license m) #f))

;; ============================================================
;; 2. Round-trip: manifest → jsexpr → manifest (equality)
;; ============================================================

(test-case "round-trip manifest→jsexpr→manifest preserves data"
  (define m (sample-manifest #:compat ">=0.4.2"
                             #:files '("main.rkt" "helpers.rkt")
                             #:checksum "abc123"
                             #:entry "main.rkt"
                             #:homepage "https://example.com"
                             #:license "MIT"))
  (define j (qpm-manifest->jsexpr m))
  (define m2 (jsexpr->qpm-manifest j))
  (check-not-false m2)
  (check-true (qpm-manifest=? m m2)))

;; ============================================================
;; 3. Round-trip: manifest → file → manifest
;; ============================================================

(test-case "round-trip manifest→file→manifest preserves data"
  (define m (sample-manifest #:compat "^0.4.0"
                             #:files '("todo.rkt")
                             #:entry "todo.rkt"
                             #:license "Apache-2.0"))
  (define tmp (make-temporary-file "q-manifest-test-~a"))
  (write-qpm-manifest m tmp)
  (define m2 (read-qpm-manifest tmp))
  (check-not-false m2)
  (check-true (qpm-manifest=? m m2))
  (delete-file tmp))

;; ============================================================
;; 4. Validation: valid manifest passes
;; ============================================================

(test-case "validation passes for valid manifest"
  (define-values (ok? errors) (validate-manifest (sample-manifest)))
  (check-true ok?)
  (check-equal? errors '()))

;; ============================================================
;; 5. Validation: empty name fails
;; ============================================================

(test-case "validation fails for empty name"
  (define-values (ok? errors)
    (validate-manifest (sample-manifest #:name "")))
  (check-false ok?)
  (check-true (ormap (λ (e) (string-contains? e "name")) errors)))

;; ============================================================
;; 6. Validation: bad version fails
;; ============================================================

(test-case "validation fails for bad version"
  (define-values (ok? errors)
    (validate-manifest (sample-manifest #:version "not-semver")))
  (check-false ok?)
  (check-true (ormap (λ (e) (string-contains? e "version")) errors)))

;; ============================================================
;; 7. Validation: bad type fails
;; ============================================================

(test-case "validation fails for bad type"
  ;; We can't pass a bad type through the constructor because the contract
  ;; enforces it. Instead we build a raw struct.
  (define bad-m (qpm-manifest "q-todo" "1.0.0" "1" 'invalid
                              "desc" "author" #f #f '() #f #f #f #f))
  (define-values (ok? errors) (validate-manifest bad-m))
  (check-false ok?)
  (check-true (ormap (λ (e) (string-contains? e "type")) errors)))

;; ============================================================
;; 8. Validation: bad api-version fails
;; ============================================================

(test-case "validation fails for non-digit api-version"
  (define-values (ok? errors)
    (validate-manifest (sample-manifest #:api-version "v1")))
  (check-false ok?)
  (check-true (ormap (λ (e) (string-contains? e "api-version")) errors)))

;; ============================================================
;; 9. qpm-manifest=? for identical manifests
;; ============================================================

(test-case "qpm-manifest=? returns #t for identical manifests"
  (define a (sample-manifest))
  (define b (sample-manifest))
  (check-true (qpm-manifest=? a b)))

;; ============================================================
;; 10. qpm-manifest=? for different manifests
;; ============================================================

(test-case "qpm-manifest=? returns #f for different manifests"
  (define a (sample-manifest #:name "q-todo"))
  (define b (sample-manifest #:name "q-other"))
  (check-false (qpm-manifest=? a b)))

;; ============================================================
;; 11. compute-manifest-checksum returns 64-char hex string
;; ============================================================

(test-case "compute-manifest-checksum returns 64-char hex string"
  (define tmpdir (make-temporary-file "q-checksum-test-~a" 'directory))
  ;; Create files
  (define f1 (build-path tmpdir "a.rkt"))
  (define f2 (build-path tmpdir "b.rkt"))
  (call-with-output-file f1 (λ (out) (display "(define x 1)" out)) #:exists 'replace)
  (call-with-output-file f2 (λ (out) (display "(define y 2)" out)) #:exists 'replace)
  ;; Write manifest
  (define m (sample-manifest #:files '("a.rkt" "b.rkt")))
  (write-qpm-manifest m (build-path tmpdir "qpm.json"))
  ;; Compute checksum
  (define cksum (compute-manifest-checksum tmpdir))
  (check-equal? (string-length cksum) 64)
  (check-true (regexp-match? #rx"^[0-9a-f]+$" cksum))
  ;; Cleanup
  (delete-file f1)
  (delete-file f2)
  (delete-file (build-path tmpdir "qpm.json"))
  (delete-directory tmpdir))

;; ============================================================
;; 12. compute-manifest-checksum is deterministic
;; ============================================================

(test-case "compute-manifest-checksum is deterministic"
  (define tmpdir (make-temporary-file "q-checksum-det-~a" 'directory))
  (define f1 (build-path tmpdir "data.txt"))
  (call-with-output-file f1 (λ (out) (display "hello world" out)) #:exists 'replace)
  (define m (sample-manifest #:files '("data.txt")))
  (write-qpm-manifest m (build-path tmpdir "qpm.json"))
  (define c1 (compute-manifest-checksum tmpdir))
  (define c2 (compute-manifest-checksum tmpdir))
  (check-equal? c1 c2)
  ;; Cleanup
  (delete-file f1)
  (delete-file (build-path tmpdir "qpm.json"))
  (delete-directory tmpdir))

;; ============================================================
;; 13. read-qpm-manifest on missing file returns #f
;; ============================================================

(test-case "read-qpm-manifest returns #f for missing file"
  (check-false (read-qpm-manifest "/tmp/q-manifest-nonexistent-xyzzy.json")))

;; ============================================================
;; 14. read-qpm-manifest on invalid JSON returns #f
;; ============================================================

(test-case "read-qpm-manifest returns #f for invalid JSON"
  (define tmp (make-temporary-file "q-manifest-bad-~a"))
  (call-with-output-file tmp (λ (out) (display "NOT VALID JSON{{{" out)) #:exists 'replace)
  (check-false (read-qpm-manifest tmp))
  (delete-file tmp))

;; ============================================================
;; 15. write-qpm-manifest creates valid JSON file
;; ============================================================

(test-case "write-qpm-manifest creates file that read-qpm-manifest can parse"
  (define m (sample-manifest #:compat ">=0.4.2"
                             #:files '("main.rkt")
                             #:entry "main.rkt"
                             #:license "MIT"))
  (define tmp (make-temporary-file "q-manifest-write-~a"))
  (write-qpm-manifest m tmp)
  (check-pred file-exists? tmp)
  (define m2 (read-qpm-manifest tmp))
  (check-not-false m2)
  (check-true (qpm-manifest=? m m2))
  (delete-file tmp))

;; ============================================================
;; 16. qpm-type? predicate
;; ============================================================

(test-case "qpm-type? accepts valid types and rejects invalid"
  (check-true (qpm-type? 'extension))
  (check-true (qpm-type? 'skill))
  (check-true (qpm-type? 'bundle))
  (check-false (qpm-type? 'invalid))
  (check-false (qpm-type? "extension"))
  (check-false (qpm-type? 42)))

;; ============================================================
;; 17. Validation: empty description fails
;; ============================================================

(test-case "validation fails for empty description"
  (define-values (ok? errors)
    (validate-manifest (sample-manifest #:description "")))
  (check-false ok?)
  (check-true (ormap (λ (e) (string-contains? e "description")) errors)))
