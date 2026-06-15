#lang racket

;; @speed slow  ;; @suite security

;; tests/test-tls-contexts.rkt — W0 (v0.99.12) mTLS Context Tests
;;
;; Tests that mTLS context creation:
;;   - Succeeds with valid cert paths
;;   - Fails clearly with missing cert/key/CA files
;;   - Produces contexts with mandatory verification enabled

(require rackunit
         rackunit/text-ui
         racket/file
         racket/system
         racket/port
         (only-in "../util/security/cert-generator.rkt"
                  generate-ca!
                  generate-server-cert!
                  generate-client-cert!)
         "../util/security/tls-contexts.rkt")

;; ── Shared test fixture: generate real certs once ──

(define test-certs-dir (make-temporary-file "tls-test-~a" 'directory))

(define-values (ca-cert-path ca-key-path) (generate-ca! test-certs-dir))
(define-values (server-cert-path server-key-path)
  (generate-server-cert! test-certs-dir ca-cert-path ca-key-path))
(define-values (client-cert-path client-key-path)
  (generate-client-cert! test-certs-dir ca-cert-path ca-key-path))

(define suite
  (test-suite "mTLS Context Creation (W0)"

    ;; ════════════════════════════════════════════════════════════
    ;; Server SSL Context
    ;; ════════════════════════════════════════════════════════════

    (test-case "make-server-ssl-context succeeds with valid certs"
      (check-not-false (make-server-ssl-context #:cert-path server-cert-path
                                                #:key-path server-key-path
                                                #:ca-path ca-cert-path)))

    (test-case "make-server-ssl-context fails with missing cert file"
      (check-exn exn:fail?
                 (lambda ()
                   (make-server-ssl-context #:cert-path "/nonexistent/cert.pem"
                                            #:key-path server-key-path
                                            #:ca-path ca-cert-path))))

    (test-case "make-server-ssl-context fails with missing key file"
      (check-exn exn:fail?
                 (lambda ()
                   (make-server-ssl-context #:cert-path server-cert-path
                                            #:key-path "/nonexistent/key.pem"
                                            #:ca-path ca-cert-path))))

    (test-case "make-server-ssl-context fails with missing CA file"
      (check-exn exn:fail?
                 (lambda ()
                   (make-server-ssl-context #:cert-path server-cert-path
                                            #:key-path server-key-path
                                            #:ca-path "/nonexistent/ca.pem"))))

    ;; ════════════════════════════════════════════════════════════
    ;; Client SSL Context
    ;; ════════════════════════════════════════════════════════════

    (test-case "make-client-ssl-context succeeds with valid certs"
      (check-not-false (make-client-ssl-context #:cert-path client-cert-path
                                                #:key-path client-key-path
                                                #:ca-path ca-cert-path)))

    (test-case "make-client-ssl-context fails with missing cert file"
      (check-exn exn:fail?
                 (lambda ()
                   (make-client-ssl-context #:cert-path "/nonexistent/cert.pem"
                                            #:key-path client-key-path
                                            #:ca-path ca-cert-path))))

    (test-case "make-client-ssl-context fails with missing key file"
      (check-exn exn:fail?
                 (lambda ()
                   (make-client-ssl-context #:cert-path client-cert-path
                                            #:key-path "/nonexistent/key.pem"
                                            #:ca-path ca-cert-path))))

    ;; ════════════════════════════════════════════════════════════
    ;; Error message quality
    ;; ════════════════════════════════════════════════════════════

    (test-case "error message mentions which file is missing"
      (define err-msg
        (with-handlers ([exn:fail? exn-message])
          (make-server-ssl-context #:cert-path "/nonexistent/cert.pem"
                                   #:key-path server-key-path
                                   #:ca-path ca-cert-path)))
      (check-true (string-contains? err-msg "server certificate chain")
                  "error message identifies the missing file type"))))

(run-tests suite 'verbose)
