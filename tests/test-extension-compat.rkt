#lang racket

;; test-extension-compat.rkt — Tests for extension compatibility (#1301)
;;
;; Tests the `compatibility` field in qpm-manifest and loader warnings.

(require rackunit
         racket/port
         json
         (only-in "../extensions/manifest.rkt"
                  qpm-manifest
                  qpm-manifest?
                  make-qpm-manifest
                  qpm-manifest-compatibility
                  qpm-manifest-compat
                  qpm-manifest-name
                  qpm-manifest->jsexpr
                  jsexpr->qpm-manifest
                  validate-manifest)
         (only-in "../extensions/loader.rkt" extension-load-error? try-load-extension))

;; ---------------------------------------------------------------------------
;; Manifest compatibility field tests
;; ---------------------------------------------------------------------------

(test-case "manifest with no compatibility loads fine"
  (define m
    (make-qpm-manifest #:name "test-ext"
                       #:version "0.1.0"
                       #:api-version "11"
                       #:type 'extension
                       #:description "Test"
                       #:author "test"))
  (check-false (qpm-manifest-compatibility m))
  (check-false (qpm-manifest-compat m)))

(test-case "manifest with valid compatibility hash"
  (define compat
    (hasheq 'min-q-version "0.10.0" 'max-q-version "0.12.0" 'tested-versions '("0.11.2")))
  (define m
    (make-qpm-manifest #:name "test-ext"
                       #:version "0.1.0"
                       #:api-version "11"
                       #:type 'extension
                       #:description "Test"
                       #:author "test"
                       #:compatibility compat))
  (check-equal? (qpm-manifest-compatibility m) compat)
  (check-equal? (hash-ref (qpm-manifest-compatibility m) 'min-q-version) "0.10.0"))

(test-case "qpm-manifest->jsexpr roundtrip includes compatibility"
  (define compat (hasheq 'min-q-version "0.11.0" 'max-q-version "" 'tested-versions '("0.11.2")))
  (define m
    (make-qpm-manifest #:name "roundtrip-ext"
                       #:version "1.0.0"
                       #:api-version "11"
                       #:type 'skill
                       #:description "Roundtrip test"
                       #:author "test"
                       #:compatibility compat))
  (define h (qpm-manifest->jsexpr m))
  (check-equal? (hash-ref h 'compatibility) compat)
  ;; Roundtrip back from hash
  (define m2 (jsexpr->qpm-manifest h))
  (check-equal? (qpm-manifest-compatibility m2) compat))

(test-case "qpm-manifest->jsexpr omits compatibility when #f"
  (define m
    (make-qpm-manifest #:name "no-compat"
                       #:version "0.1.0"
                       #:api-version "11"
                       #:type 'extension
                       #:description "No compat"
                       #:author "test"))
  (define h (qpm-manifest->jsexpr m))
  (check-false (hash-has-key? h 'compatibility)))

(test-case "validate-manifest rejects non-hash compatibility via contract"
  ;; The contract on make-qpm-manifest rejects non-hash values,
  ;; so we test that the contract is enforced.
  (check-exn exn:fail:contract?
             (λ ()
               (make-qpm-manifest #:name "bad-compat"
                                  #:version "0.1.0"
                                  #:api-version "11"
                                  #:type 'extension
                                  #:description "Bad compat"
                                  #:author "test"
                                  #:compatibility "not-a-hash"))))

(test-case "validate-manifest accepts valid compatibility"
  (define compat
    (hasheq 'min-q-version "0.10.0" 'max-q-version "0.12.0" 'tested-versions '("0.11.0")))
  (define m
    (make-qpm-manifest #:name "good-compat"
                       #:version "0.1.0"
                       #:api-version "11"
                       #:type 'extension
                       #:description "Good compat"
                       #:author "test"
                       #:compatibility compat))
  (define-values (valid? errors) (validate-manifest m))
  (check-true valid?)
  (check-equal? errors '()))

(test-case "try-load-extension returns error for nonexistent path"
  (define result (try-load-extension "/nonexistent/path/to/extension.rkt"))
  (check-true (extension-load-error? result)))
