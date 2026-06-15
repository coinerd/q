#lang racket

;; @speed slow  ;; @suite security

;; tests/test-cert-generator.rkt — W0 (v0.99.12) Certificate Generation Tests
;;
;; Tests that certificate generation produces valid PEM files that:
;;   - Can be generated from scratch
;;   - Are valid PEM format (openssl verifies them)
;;   - Work with TLS context creation (round-trip)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/system
         racket/port
         (only-in "../util/security/cert-generator.rkt"
                  generate-ca!
                  generate-server-cert!
                  generate-client-cert!
                  generate-cert-set!)
         (only-in "../util/security/tls-contexts.rkt"
                  make-server-ssl-context
                  make-client-ssl-context))

;; ── Helpers ──

(define (file-contains-pem? path)
  (and (file-exists? path)
       (regexp-match? #rx"BEGIN (CERTIFICATE|PRIVATE KEY)" (file->string path #:mode 'text))))

(define (openssl-verifies-cert? cert-path ca-path)
  (define result
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system/exit-code
       (format "openssl verify -CAfile ~a ~a" (path->string ca-path) (path->string cert-path)))))
  (= result 0))

;; ── Shared fixture: generate cert set once ──

(define test-dir (make-temporary-file "certgen-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-dir))

(define suite
  (test-suite "Certificate Generation (W0)"

    ;; ════════════════════════════════════════════════════════════
    ;; CA Generation
    ;; ════════════════════════════════════════════════════════════

    (test-case "generate-ca! produced valid PEM files"
      (check-true (file-contains-pem? (hash-ref cert-paths 'ca-cert)) "ca.pem is a valid PEM cert")
      (check-true (file-contains-pem? (hash-ref cert-paths 'ca-key)) "ca-key.pem is a valid PEM key")
      (check-true (openssl-verifies-cert? (hash-ref cert-paths 'ca-cert)
                                          (hash-ref cert-paths 'ca-cert))
                  "CA cert verifies against itself"))

    ;; ════════════════════════════════════════════════════════════
    ;; Server Certificate Generation
    ;; ════════════════════════════════════════════════════════════

    (test-case "generate-server-cert! produced cert signed by CA"
      (check-true (file-contains-pem? (hash-ref cert-paths 'server-cert))
                  "server.pem is a valid PEM cert")
      (check-true (file-contains-pem? (hash-ref cert-paths 'server-key))
                  "server-key.pem is a valid PEM key")
      (check-true (openssl-verifies-cert? (hash-ref cert-paths 'server-cert)
                                          (hash-ref cert-paths 'ca-cert))
                  "server cert verifies against CA"))

    ;; ════════════════════════════════════════════════════════════
    ;; Client Certificate Generation
    ;; ════════════════════════════════════════════════════════════

    (test-case "generate-client-cert! produced cert signed by CA"
      (check-true (file-contains-pem? (hash-ref cert-paths 'client-cert))
                  "client.pem is a valid PEM cert")
      (check-true (file-contains-pem? (hash-ref cert-paths 'client-key))
                  "client-key.pem is a valid PEM key")
      (check-true (openssl-verifies-cert? (hash-ref cert-paths 'client-cert)
                                          (hash-ref cert-paths 'ca-cert))
                  "client cert verifies against CA"))

    ;; ════════════════════════════════════════════════════════════
    ;; Full Certificate Set
    ;; ════════════════════════════════════════════════════════════

    (test-case "generate-cert-set! produced all 6 files"
      (for ([key '(ca-cert ca-key server-cert server-key client-cert client-key)])
        (check-true (file-exists? (hash-ref cert-paths key)) (format "~a file exists" key))))

    ;; ════════════════════════════════════════════════════════════
    ;; Round-trip: generated certs work with TLS contexts
    ;; ════════════════════════════════════════════════════════════

    (test-case "generated certs create valid TLS contexts"
      ;; Server context
      (check-not-false (make-server-ssl-context #:cert-path (hash-ref cert-paths 'server-cert)
                                                #:key-path (hash-ref cert-paths 'server-key)
                                                #:ca-path (hash-ref cert-paths 'ca-cert)))
      ;; Client context
      (check-not-false (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                                                #:key-path (hash-ref cert-paths 'client-key)
                                                #:ca-path (hash-ref cert-paths 'ca-cert))))))

(run-tests suite 'verbose)
