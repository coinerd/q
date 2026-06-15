#lang racket/base

;; util/security/cert-generator.rkt — CA + client/server certificate generation
;; STABILITY: evolving
;;
;; W0 (v0.99.12): Generates self-signed CA and CA-signed client/server
;; certificate pairs using the `openssl` CLI. These certs are used for
;; mutual-TLS (mTLS) between the orchestrator and executor nodes.
;;
;; All certs use RSA 4096-bit keys with 365-day validity.
;; Output files follow the naming convention:
;;   ca.pem / ca-key.pem
;;   server.pem / server-key.pem
;;   client.pem / client-key.pem

(require racket/contract
         racket/file
         racket/string
         racket/system
         racket/port)

;; ============================================================
;; Helpers
;; ============================================================

(define (run-openssl! args output-dir)
  (define result
    (parameterize ([current-directory output-dir]
                   [current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system/exit-code (format "openssl ~a" (string-join args " ")))))
  (unless (= result 0)
    (raise (exn:fail (format "cert-generator: openssl failed with exit code ~a (args: ~a)"
                             result
                             (string-join args " "))
                     (current-continuation-marks))))
  result)

(define (ensure-output-dir! output-dir)
  (make-directory* output-dir)
  output-dir)

;; ============================================================
;; CA Generation
;; ============================================================

;; Generate a self-signed CA certificate and private key.
;; Produces: ca.pem (cert) and ca-key.pem (private key) in output-dir.
(define (generate-ca! output-dir)
  (ensure-output-dir! output-dir)
  (run-openssl! '("req" "-x509"
                        "-newkey"
                        "rsa:4096"
                        "-days"
                        "365"
                        "-nodes"
                        "-keyout"
                        "ca-key.pem"
                        "-out"
                        "ca.pem"
                        "-subj"
                        "/CN=q-mtls-ca/O=q-agent")
                output-dir)
  (values (build-path output-dir "ca.pem") (build-path output-dir "ca-key.pem")))

;; ============================================================
;; Server Certificate Generation
;; ============================================================

;; Generate a server certificate signed by the CA.
;; Produces: server.pem (cert) and server-key.pem (private key) in output-dir.
(define (generate-server-cert! output-dir ca-cert-path ca-key-path)
  (ensure-output-dir! output-dir)
  ;; Step 1: Generate private key and CSR
  (run-openssl! '("req" "-newkey"
                        "rsa:4096"
                        "-nodes"
                        "-keyout"
                        "server-key.pem"
                        "-out"
                        "server.csr"
                        "-subj"
                        "/CN=q-executor-server/O=q-agent")
                output-dir)
  ;; Step 2: Sign CSR with CA
  (run-openssl! (list "x509"
                      "-req"
                      "-in"
                      "server.csr"
                      "-CA"
                      (path->string ca-cert-path)
                      "-CAkey"
                      (path->string ca-key-path)
                      "-CAcreateserial"
                      "-out"
                      "server.pem"
                      "-days"
                      "365")
                output-dir)
  (values (build-path output-dir "server.pem") (build-path output-dir "server-key.pem")))

;; ============================================================
;; Client Certificate Generation
;; ============================================================

;; Generate a client certificate signed by the CA.
;; Produces: client.pem (cert) and client-key.pem (private key) in output-dir.
(define (generate-client-cert! output-dir ca-cert-path ca-key-path)
  (ensure-output-dir! output-dir)
  ;; Step 1: Generate private key and CSR
  (run-openssl! '("req" "-newkey"
                        "rsa:4096"
                        "-nodes"
                        "-keyout"
                        "client-key.pem"
                        "-out"
                        "client.csr"
                        "-subj"
                        "/CN=q-orchestrator-client/O=q-agent")
                output-dir)
  ;; Step 2: Sign CSR with CA
  (run-openssl! (list "x509"
                      "-req"
                      "-in"
                      "client.csr"
                      "-CA"
                      (path->string ca-cert-path)
                      "-CAkey"
                      (path->string ca-key-path)
                      "-CAcreateserial"
                      "-out"
                      "client.pem"
                      "-days"
                      "365")
                output-dir)
  (values (build-path output-dir "client.pem") (build-path output-dir "client-key.pem")))

;; ============================================================
;; Full Certificate Set Generation
;; ============================================================

;; Generate a complete CA + server + client certificate set.
;; Returns a hash with all paths.
(define (generate-cert-set! output-dir)
  (ensure-output-dir! output-dir)
  (define-values (ca-cert ca-key) (generate-ca! output-dir))
  (define-values (server-cert server-key) (generate-server-cert! output-dir ca-cert ca-key))
  (define-values (client-cert client-key) (generate-client-cert! output-dir ca-cert ca-key))
  (hasheq 'ca-cert
          ca-cert
          'ca-key
          ca-key
          'server-cert
          server-cert
          'server-key
          server-key
          'client-cert
          client-cert
          'client-key
          client-key))

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out [generate-ca! (-> path-string? (values path? path?))]
                       [generate-server-cert! (-> path-string? path? path? (values path? path?))]
                       [generate-client-cert! (-> path-string? path? path? (values path? path?))]
                       [generate-cert-set! (-> path-string? hash?)]))
