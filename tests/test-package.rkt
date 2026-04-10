#lang racket

;; tests/test-package.rkt — tests for runtime/package.rkt

(require rackunit
         racket/file
         racket/port
         json
         "../extensions/manifest.rkt"
         "../runtime/package.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (with-test-packages-dir thunk)
  (define tmpdir (make-temporary-file "q-pkg-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda () (parameterize ([current-packages-dir tmpdir]) (thunk)))
    (lambda ()
      (when (directory-exists? tmpdir)
        (delete-directory/files tmpdir)))))

(define (make-test-source-package #:name [name "q-test-pkg"])
  (define srcdir (make-temporary-file "q-pkg-src-~a" 'directory))
  (call-with-output-file (build-path srcdir "main.rkt")
    (lambda (out) (display "#lang racket/base\n(define x 1)\n" out))
    #:exists 'replace)
  ;; Also create a subdirectory file to test nested copies
  (make-directory* (build-path srcdir "lib"))
  (call-with-output-file (build-path srcdir "lib" "helper.rkt")
    (lambda (out) (display "#lang racket/base\n(define y 2)\n" out))
    #:exists 'replace)
  (define m (make-qpm-manifest #:name name #:version "1.0.0" #:api-version "1"
               #:type 'extension #:description "Test package" #:author "tester"
               #:files '("main.rkt" "lib/helper.rkt") #:entry "main.rkt"))
  (write-qpm-manifest m (build-path srcdir "qpm.json"))
  srcdir)

;; Cleanup a source package dir
(define (cleanup-source-dir srcdir)
  (when (directory-exists? srcdir)
    (delete-directory/files srcdir)))

;; ============================================================
;; Tests
;; ============================================================

(test-case "list-packages returns empty for fresh dir"
  (with-test-packages-dir
   (lambda ()
     (check-equal? (list-packages) '()))))

(test-case "install-package-from-dir succeeds with valid source"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (define result (install-package-from-dir srcdir))
         (check-pred qpm-package? result)
         (check-equal? (qpm-manifest-name (qpm-package-manifest result)) "q-test-pkg")
         (check-equal? (qpm-package-status result) 'installed))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "install-package-from-dir fails for dir without qpm.json"
  (define srcdir (make-temporary-file "q-pkg-src-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (define result (install-package-from-dir srcdir))
         (check-pred string? result)
         (check-true (string-contains? result "no qpm.json")))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "package-installed? returns #f before install"
  (with-test-packages-dir
   (lambda ()
     (check-false (package-installed? "q-test-pkg")))))

(test-case "package-installed? returns #t after install"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir)
         (check-true (package-installed? "q-test-pkg")))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "package-info returns #f for unknown"
  (with-test-packages-dir
   (lambda ()
     (check-false (package-info "no-such-package")))))

(test-case "package-info returns info after install"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir)
         (define info (package-info "q-test-pkg"))
         (check-pred qpm-package? info)
         (check-equal? (qpm-manifest-name (qpm-package-manifest info)) "q-test-pkg"))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "remove-package returns #t after install"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir)
         (check-true (remove-package "q-test-pkg"))
         (check-false (package-installed? "q-test-pkg")))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "remove-package returns #f for unknown"
  (with-test-packages-dir
   (lambda ()
     (check-false (remove-package "no-such-package")))))

(test-case "list-packages returns installed package"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir)
         (define pkgs (list-packages))
         (check-equal? (length pkgs) 1)
         (check-equal? (qpm-manifest-name (qpm-package-manifest (car pkgs))) "q-test-pkg"))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "install-package-from-dir rejects invalid manifest"
  ;; Create a source dir with a qpm.json that has invalid fields
  (define srcdir (make-temporary-file "q-pkg-src-~a" 'directory))
  (call-with-output-file (build-path srcdir "qpm.json")
    (lambda (out)
      (write-json (hasheq 'name ""
                           'version "not-semver"
                           'api_version "abc"
                           'type "unknown"
                           'description ""
                           'author ""
                           'files '())
                  out))
    #:exists 'replace)
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (define result (install-package-from-dir srcdir))
         (check-pred string? result)
         (check-true (string-contains? result "error: invalid manifest")))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "install-package-from-dir copies all listed files"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (define pkg (install-package-from-dir srcdir))
         (check-pred qpm-package? pkg)
         (define inst-dir (qpm-package-install-dir pkg))
         ;; main.rkt should exist
         (check-true (file-exists? (build-path inst-dir "main.rkt")))
         ;; lib/helper.rkt should exist
         (check-true (file-exists? (build-path inst-dir "lib" "helper.rkt")))
         ;; qpm.json should exist
         (check-true (file-exists? (build-path inst-dir "qpm.json"))))))
    (lambda () (cleanup-source-dir srcdir))))

(test-case "install-package-from-dir handles multiple packages"
  (define srcdir-a (make-test-source-package #:name "pkg-alpha"))
  (define srcdir-b (make-test-source-package #:name "pkg-bravo"))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir-a)
         (install-package-from-dir srcdir-b)
         (define pkgs (list-packages))
         (check-equal? (length pkgs) 2)
         ;; Sorted by name: alpha before bravo
         (check-equal? (qpm-manifest-name (qpm-package-manifest (first pkgs))) "pkg-alpha")
         (check-equal? (qpm-manifest-name (qpm-package-manifest (second pkgs))) "pkg-bravo"))))
    (lambda ()
      (cleanup-source-dir srcdir-a)
      (cleanup-source-dir srcdir-b))))

(test-case "reinstall overwrites existing package"
  (define srcdir (make-test-source-package))
  (dynamic-wind
    void
    (lambda ()
      (with-test-packages-dir
       (lambda ()
         (install-package-from-dir srcdir)
         ;; Install again — should succeed
         (define result (install-package-from-dir srcdir))
         (check-pred qpm-package? result)
         (check-true (package-installed? "q-test-pkg")))))
    (lambda () (cleanup-source-dir srcdir))))
