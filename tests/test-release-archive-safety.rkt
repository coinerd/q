#lang racket

;; @suite release-smoke
;; @speed fast
;; @boundary integration
;; W2 (#9072): fail-closed schema and malicious archive regressions.

(require rackunit
         openssl
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/system
         (only-in file/sha1 bytes->hex-string)
         (only-in "../scripts/gen-release-manifest.rkt"
                  manifest
                  manifest-asset
                  manifest-trace
                  manifest->json-string))

(define-runtime-path repo-root "..")
(define-runtime-path verifier "../scripts/verify-release-bundle.rkt")
(define full-sha "abcdef0123456789abcdef0123456789abcdef01")
(define tag-object "fedcba9876543210fedcba9876543210fedcba98")

(define (digest path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

(define (manifest-json tarball)
  (manifest->json-string
   (manifest "1.2.3"
             "v1.2.3"
             full-sha
             "2026-07-29"
             (list (manifest-asset "q-1.2.3.tar.gz" (file-size tarball) (digest tarball)))
             "8.10"
             "racket main.rkt --version"
             (manifest-trace "v1.2.3" full-sha tag-object full-sha #t))))

(define (verify-exit tarball manifest-path)
  (parameterize ([current-directory repo-root])
    (system*/exit-code (find-executable-path "racket")
                       verifier
                       "--version"
                       "1.2.3"
                       "--tag"
                       "v1.2.3"
                       "--commit"
                       full-sha
                       "--tag-object"
                       tag-object
                       (path->string tarball)
                       (path->string manifest-path))))

(define (write-manifest! tarball manifest-path [transform values])
  (call-with-output-file manifest-path
                         #:exists 'truncate
                         (lambda (out) (display (transform (manifest-json tarball)) out))))

(test-case "verifier rejects archive traversal entries"
  (define dir (make-temporary-file "q-archive-traversal-~a" 'directory))
  (define payload (build-path dir "payload"))
  (define tarball (build-path dir "q-1.2.3.tar.gz"))
  (define manifest-path (build-path dir "release-manifest.json"))
  (call-with-output-file payload (lambda (out) (display "x" out)))
  (check-equal? (system*/exit-code (find-executable-path "tar")
                                   "czf"
                                   tarball
                                   "--transform=s|payload|../escape|"
                                   "-C"
                                   dir
                                   "payload")
                0)
  (write-manifest! tarball manifest-path)
  (dynamic-wind void
                (lambda () (check-not-equal? (verify-exit tarball manifest-path) 0))
                (lambda () (delete-directory/files dir))))

(test-case "verifier rejects escaping symlink entries"
  (define dir (make-temporary-file "q-archive-link-~a" 'directory))
  (define root (build-path dir "q"))
  (define tarball (build-path dir "q-1.2.3.tar.gz"))
  (define manifest-path (build-path dir "release-manifest.json"))
  (make-directory root)
  (make-file-or-directory-link "../../outside" (build-path root "escape-link"))
  (check-equal? (system*/exit-code (find-executable-path "tar") "czf" tarball "-C" dir "q") 0)
  (write-manifest! tarball manifest-path)
  (dynamic-wind void
                (lambda () (check-not-equal? (verify-exit tarball manifest-path) 0))
                (lambda () (delete-directory/files dir))))

(test-case "verifier rejects duplicate normalized archive paths"
  (define dir (make-temporary-file "q-archive-duplicate-~a" 'directory))
  (define root (build-path dir "q"))
  (define tarball (build-path dir "q-1.2.3.tar.gz"))
  (define manifest-path (build-path dir "release-manifest.json"))
  (make-directory root)
  (call-with-output-file (build-path root "same") (lambda (out) (display "x" out)))
  (check-equal?
   (system*/exit-code (find-executable-path "tar") "czf" tarball "-C" dir "q/same" "q/same")
   0)
  (write-manifest! tarball manifest-path)
  (dynamic-wind void
                (lambda () (check-not-equal? (verify-exit tarball manifest-path) 0))
                (lambda () (delete-directory/files dir))))

(test-case "verifier rejects unknown fields and tampered raw tag"
  (define dir (make-temporary-file "q-schema-unknown-~a" 'directory))
  (define root (build-path dir "q"))
  (define tarball (build-path dir "q-1.2.3.tar.gz"))
  (define manifest-path (build-path dir "release-manifest.json"))
  (make-directory root)
  (call-with-output-file (build-path root "README") (lambda (out) (display "x" out)))
  (check-equal? (system*/exit-code (find-executable-path "tar") "czf" tarball "-C" dir "q") 0)
  (dynamic-wind
   void
   (lambda ()
     (write-manifest! tarball
                      manifest-path
                      (lambda (json)
                        (string-replace json "{\"assets\"" "{\"unexpected\":true,\"assets\"")))
     (check-not-equal? (verify-exit tarball manifest-path) 0)
     (write-manifest! tarball
                      manifest-path
                      (lambda (json) (string-replace json "\"tag\":\"v1.2.3\"" "\"tag\":\"v9.9.9\"")))
     (check-not-equal? (verify-exit tarball manifest-path) 0))
   (lambda () (delete-directory/files dir))))
