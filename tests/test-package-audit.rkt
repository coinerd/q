#lang racket

;; tests/test-package-audit.rkt -- TDD tests for util/package-audit.rkt
;;
;; Issue #54: Install-time warnings for code-executing packages.

(require rackunit
         racket/file
         racket/port
         "../util/package-audit.rkt"
         "../extensions/manifest.rkt")

;; ============================================================
;; Helpers: create temporary package directories
;; ============================================================

(define (make-tmp-dir)
  (make-temporary-directory "q-audit-test-~a"))

(define (write-qpm-json! dir
                          #:name [name "test-pkg"]
                          #:version [version "1.0.0"]
                          #:api-version [api-version "1"]
                          #:type [type "extension"]
                          #:description [description "Test package"]
                          #:author [author "tester"]
                          #:entry [entry #f]
                          #:checksum [checksum #f]
                          #:files [files '()])
  ;; Use manifest module for reliable JSON output
  (define m (make-qpm-manifest #:name name
                               #:version version
                               #:api-version api-version
                               #:type (string->symbol type)
                               #:description description
                               #:author author
                               #:files files
                               #:checksum checksum
                               #:entry entry))
  (write-qpm-manifest m (build-path dir "qpm.json")))

(define (write-rkt-file! dir filename content)
  (call-with-output-file (build-path dir filename)
    (lambda (out) (display content out))
    #:exists 'replace))

;; ============================================================
;; 1. Low-risk package (skill, no risky patterns)
;; ============================================================

(test-case "low-risk skill package"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "skill")
      (write-rkt-file! dir "helper.rkt"
                       "#lang racket/base\n(define (hello) \"world\")\n")
      (define result (audit-package dir))
      (check-equal? (audit-result-risk-level result) 'low)
      (check-true (null? (filter (lambda (f)
                                   (eq? (finding-severity f) 'high))
                                 (audit-result-findings result)))))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 2. Extension baseline → medium risk
;; ============================================================

(test-case "extension baseline is medium"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "main.rkt"
                       "#lang racket/base\n(define x 42)\n")
      (define result (audit-package dir))
      (check-equal? (audit-result-risk-level result) 'medium))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 3. Package with subprocess patterns → high finding
;; ============================================================

(test-case "subprocess patterns produce high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "exec.rkt"
                       "#lang racket/base\n(require racket/process)\n(subprocess #f out err \"ls\")\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1)
                  (format "expected >=1 high finding, got ~a" highs))
      (check-true (string-contains? (finding-message (car highs)) "subprocess")))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 4. Package with network patterns → high finding
;; ============================================================

(test-case "network patterns produce high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "net.rkt"
                       "#lang racket/base\n(require net/http)\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "network"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 5. Package with file-write patterns → medium finding
;; ============================================================

(test-case "file-write patterns produce medium finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "skill")
      (write-rkt-file! dir "writer.rkt"
                       "#lang racket/base\n(call-with-output-file \"/tmp/x\" (lambda (p) (write-bytes #\"hi\" p)))\n")
      (define result (audit-package dir))
      (define meds (filter (lambda (f) (eq? (finding-severity f) 'medium))
                           (audit-result-findings result)))
      (check-true (>= (length meds) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "file write"))
                         meds)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 6. classify-risk: multiple medium findings → high
;; ============================================================

(test-case "classify-risk: two medium findings escalate to high"
  (define findings
    (list (finding 'medium "file write operations in a.rkt")
          (finding 'medium "file write operations in b.rkt")))
  (check-equal? (classify-risk findings 'extension) 'high))

;; ============================================================
;; 7. format-audit-report produces non-empty string
;; ============================================================

(test-case "format-audit-report returns non-empty string"
  (define result (audit-result 'high
                               (list (finding 'high "subprocess execution"))
                               #t))
  (define report (format-audit-report result))
  (check-true (and (string? report) (> (string-length report) 0))))

;; ============================================================
;; 8. format-audit-warning includes risk level
;; ============================================================

(test-case "format-audit-warning includes risk level"
  (define result (audit-result 'high
                               (list (finding 'high "subprocess execution"))
                               #t))
  (define warning (format-audit-warning result))
  (check-true (string-contains? warning "high")))

;; ============================================================
;; 9. Package with no manifest → handles gracefully
;; ============================================================

(test-case "missing manifest handled gracefully"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      ;; No qpm.json written
      (define result (audit-package dir))
      (check-equal? (audit-result-risk-level result) 'medium)
      (check-true (ormap (lambda (f)
                           (string-contains? (finding-message f) "manifest"))
                         (audit-result-findings result))))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 10. Entry point adds finding
;; ============================================================

(test-case "entry point in manifest adds finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension" #:entry "main.rkt")
      (write-rkt-file! dir "main.rkt" "#lang racket/base\n")
      (define result (audit-package dir))
      (check-true (ormap (lambda (f)
                           (string-contains? (finding-message f) "entry"))
                         (audit-result-findings result))))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 11. eval pattern detected
;; ============================================================

(test-case "eval pattern produces high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "evl.rkt"
                       "#lang racket/base\n(eval \"(+ 1 2)\")\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "eval/compile"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 12. dynamic-require pattern detected
;; ============================================================

(test-case "dynamic-require pattern produces high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "dyn.rkt"
                       "#lang racket/base\n(dynamic-require 'racket/list 'first)\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "dynamic-require"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 13. FFI pattern detected
;; ============================================================

(test-case "ffi pattern produces high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "ffi.rkt"
                       "#lang racket/base\n(require ffi/unsafe)\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "FFI"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 14. putenv pattern detected
;; ============================================================

(test-case "env-modification pattern produces high finding"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      (write-rkt-file! dir "env.rkt"
                       "#lang racket/base\n(putenv \"FOO\" \"bar\")\n")
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) "environment"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 15. .zo file flagged as unscannable
;; ============================================================

(test-case ".zo file flagged as unscannable"
  (define dir (make-tmp-dir))
  (dynamic-wind
    void
    (lambda ()
      (write-qpm-json! dir #:type "extension")
      ;; Write a fake .zo file (binary content)
      (call-with-output-file (build-path dir "compiled.zo")
        (lambda (out) (write-bytes #"\0\1\2\3" out))
        #:exists 'replace)
      (define result (audit-package dir))
      (define highs (filter (lambda (f) (eq? (finding-severity f) 'high))
                            (audit-result-findings result)))
      (check-true (>= (length highs) 1))
      (check-true (ormap (lambda (f) (string-contains? (finding-message f) ".zo"))
                         highs)))
    (lambda ()
      (delete-directory/files dir #:must-exist? #f))))
