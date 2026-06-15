#lang racket/base

;; q/cli/generate-certificates.rkt — q generate-certificates command
;;
;; W0 (v0.99.12): Generates a complete mTLS certificate set (CA + server +
;; client) for use with distributed execution.
;;
;; Usage:
;;   q generate-certificates --output-dir ~/.q/certs
;;   q generate-certificates --output-dir ~/.q/certs --force

(require racket/contract
         racket/cmdline
         racket/file
         racket/port
         "../util/security/cert-generator.rkt")

;; ── Main ──

(define (run-generate-certificates args)
  (define output-dir (make-parameter "~/.q/certs"))
  (define force? (make-parameter #f))
  (command-line #:program "q generate-certificates"
                #:argv args
                #:once-each
                [("--output-dir" "-o") dir "Output directory for certificates" (output-dir dir)]
                [("--force" "-f") "Overwrite existing certificates" (force? #t)]
                #:args ()
                (void))
  (define dir (output-dir))
  (define resolved-dir
    (if (string-prefix? dir "~/")
        (build-path (find-system-path 'home-dir) (substring dir 2))
        dir))
  ;; Check for existing certs
  (define ca-path (build-path resolved-dir "ca.pem"))
  (when (and (file-exists? ca-path) (not (force?)))
    (displayln (format "Error: ca.pem already exists in ~a. Use --force to overwrite." resolved-dir))
    (exit 1))
  (printf "Generating mTLS certificate set in ~a...~n" resolved-dir)
  (define paths (generate-cert-set! resolved-dir))
  (printf "✓ CA certificate: ~a~n" (hash-ref paths 'ca-cert))
  (printf "✓ Server certificate: ~a~n" (hash-ref paths 'server-cert))
  (printf "✓ Client certificate: ~a~n" (hash-ref paths 'client-cert))
  (displayln "")
  (displayln "Deployment instructions:")
  (displayln "  1. Copy ca.pem, server.pem, server-key.pem to the executor node")
  (displayln "  2. Copy ca.pem, client.pem, client-key.pem to the orchestrator node")
  (displayln "  3. Configure mas.broker.cert-dir to point to the cert directory")
  (displayln "  4. Set mas.broker.capability-secret to a shared secret")
  (displayln "  5. Enable mas.broker.enabled = true"))

(provide run-generate-certificates)
