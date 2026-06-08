#lang racket

;; test-credential-protocol.rkt — Verify credential backend protocol contracts (F4)
;;
;; Tests that all backends conform to the credential-backend interface
;; and that contract-out guards catch invalid inputs.

(require rackunit
         racket/file
         (only-in "../runtime/credentials/protocol.rkt"
                  credential-backend?
                  backend-name
                  backend-store!
                  backend-load
                  backend-delete!
                  backend-list-providers
                  backend-available?)
         (only-in "../runtime/credentials/memory-backend.rkt" make-memory-credential-backend)
         (only-in "../runtime/credentials/file-backend.rkt" make-file-credential-backend)
         (only-in "../runtime/credentials/chained-backend.rkt" make-chained-credential-backend))

;; ═══════════════════════════════════════════════════════════════════
;; Protocol conformance — each backend must satisfy these checks
;; ═══════════════════════════════════════════════════════════════════

(define (check-backend-conformance backend label)
  (test-case (format "~a backend conformance" label)
    (check-true (credential-backend? backend) (format "~a: credential-backend?" label))
    (check-true (string? (backend-name backend)) (format "~a: backend-name returns string" label))
    ;; Round-trip: store → load → delete → verify gone
    (backend-store! backend "test-provider-key" "test-api-key-12345")
    (define loaded (backend-load backend "test-provider-key"))
    (check-not-false loaded (format "~a: load after store returns truthy" label))
    (check-true (hash? loaded) (format "~a: load returns hash" label))
    ;; List should include the stored provider
    (define keys (backend-list-providers backend))
    (check-true (list? keys) (format "~a: list-providers returns list" label))
    (check-not-false (member "test-provider-key" keys)
                     (format "~a: stored key appears in list" label))
    ;; Delete and verify
    (backend-delete! backend "test-provider-key")
    (define after-delete (backend-load backend "test-provider-key"))
    (check-false after-delete (format "~a: load after delete returns #f" label))
    ;; Available check
    (check-true (boolean? (backend-available? backend))
                (format "~a: available? returns boolean" label))))

;; ═══════════════════════════════════════════════════════════════════
;; Test each backend
;; ═══════════════════════════════════════════════════════════════════

(define temp-dir (make-temporary-file "credential-test-~a" 'directory))
(define temp-cred-file (build-path temp-dir "credentials.json"))

(check-backend-conformance (make-memory-credential-backend) "memory")

(check-backend-conformance (make-file-credential-backend temp-cred-file) "file")

(check-backend-conformance (make-chained-credential-backend (list (make-memory-credential-backend)))
                           "chained-memory")

;; ═══════════════════════════════════════════════════════════════════
;; Error handling: missing key returns #f
;; ═══════════════════════════════════════════════════════════════════

(test-case "memory-backend: load missing key returns #f"
  (define be (make-memory-credential-backend))
  (check-false (backend-load be "nonexistent-key")))

(test-case "memory-backend: delete missing key succeeds silently"
  (define be (make-memory-credential-backend))
  (check-not-exn (λ () (backend-delete! be "nonexistent-key"))))

;; ═══════════════════════════════════════════════════════════════════
;; Contract guards: invalid inputs raise contract errors
;; ═══════════════════════════════════════════════════════════════════

(test-case "backend-name: rejects non-backend"
  (check-exn exn:fail:contract? (λ () (backend-name "not-a-backend"))))

(test-case "backend-store!: rejects non-backend"
  (check-exn exn:fail:contract? (λ () (backend-store! "not-a-backend" "key" "val"))))

(test-case "backend-load: rejects non-backend"
  (check-exn exn:fail:contract? (λ () (backend-load "not-a-backend" "key"))))

;; Cleanup
(with-handlers ([exn:fail? void])
  (delete-directory/files temp-dir))
